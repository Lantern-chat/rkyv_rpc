//! Defines an rkyv codec for use with tokio.
//!
//! While not truly zero-copy, as it needs to create an aligned buffer to decode into, it is
//! zero-deserialization, as the archived data is directly accessible from the buffer.

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    IoError(#[from] std::io::Error),

    #[error(transparent)]
    CheckError(#[from] RancorError),
}

use core::{marker::PhantomData, mem::size_of, ops::Deref};

use tokio_util::{
    bytes::BytesMut,
    codec::{Decoder, Encoder, LengthDelimitedCodec},
};

use rkyv::{
    api::high::{HighSerializer, HighValidator},
    bytecheck::CheckBytes,
    rancor::Error as RancorError,
    ser::allocator::{Arena, ArenaHandle},
    validation::{archive::ArchiveValidator, shared::SharedValidator, Validator},
    Archive, Archived, Serialize,
};

/// Zero-cost wrapper around an rkyv archive that has been checked for validity.
pub struct CheckedArchive<T> {
    _marker: std::marker::PhantomData<T>,

    #[cfg(not(feature = "unaligned"))]
    bytes: rkyv::util::AlignedVec,

    #[cfg(feature = "unaligned")]
    bytes: tokio_util::bytes::Bytes,
}

impl<T> Deref for CheckedArchive<T>
where
    T: Archive,
{
    type Target = Archived<T>;

    fn deref(&self) -> &Self::Target {
        // SAFETY: This is safe because the buffer is aligned and the root type
        // was checked when when it was decoded.
        unsafe { rkyv::api::access_unchecked(&self.bytes) }
    }
}

/// A codec that decodes rkyv archives.
///
/// See [`RkyvCodec`] for a codec that encodes and decodes rkyv archives.
///
/// For decoding, the archived data will be validated before being returned.
pub struct RkyvDecoder<T> {
    inner: LengthDelimitedCodec,
    _marker: PhantomData<T>,
}

/// A codec that encodes and decodes rkyv archives.
///
/// This will reuse the same arena for encoding, so it's best to use the same codec for encoding and
/// decoding.
///
/// For decoding, the archived data will be validated before being returned.
pub struct RkyvCodec<T> {
    inner: LengthDelimitedCodec,
    arena: Arena,
    _marker: PhantomData<T>,
}

/// Creates the standard underlying length-delimited codec for rkyv.
fn codec() -> LengthDelimitedCodec {
    LengthDelimitedCodec::builder()
        .big_endian()
        .max_frame_length(u32::MAX as _)
        .length_field_type::<u32>()
        .new_codec()
}

impl<T> Default for RkyvCodec<T> {
    fn default() -> Self {
        Self {
            inner: codec(),
            arena: Arena::new(),
            _marker: PhantomData,
        }
    }
}

impl<T> Default for RkyvDecoder<T> {
    fn default() -> Self {
        Self {
            inner: codec(),
            _marker: PhantomData,
        }
    }
}

impl<T> From<LengthDelimitedCodec> for RkyvDecoder<T> {
    fn from(inner: LengthDelimitedCodec) -> Self {
        Self {
            inner,
            _marker: PhantomData,
        }
    }
}

impl<T> From<LengthDelimitedCodec> for RkyvCodec<T> {
    fn from(inner: LengthDelimitedCodec) -> Self {
        Self {
            inner,
            arena: Arena::new(),
            _marker: PhantomData,
        }
    }
}

fn decode_owned<T>(
    inner: &mut LengthDelimitedCodec,
    src: &mut BytesMut,
) -> Result<Option<CheckedArchive<T>>, Error>
where
    T: Archive,
    Archived<T>: for<'a> CheckBytes<HighValidator<'a, RancorError>>,
{
    let bytes = match inner.decode(src)? {
        None => return Ok(None),

        #[cfg(feature = "unaligned")]
        Some(bytes) => bytes.freeze(),

        #[cfg(not(feature = "unaligned"))]
        Some(bytes) => {
            // copy over bytes to ensure alignment
            let mut aligned = rkyv::util::AlignedVec::with_capacity(bytes.len());
            aligned.extend_from_slice(&bytes);
            aligned
        }
    };

    // rkyv::access but without the actual access
    rkyv::api::check_pos_with_context::<Archived<T>, _, RancorError>(
        &bytes,
        rkyv::api::root_position::<Archived<T>>(bytes.len()),
        &mut Validator::new(ArchiveValidator::new(&bytes), SharedValidator::new()),
    )?;

    Ok(Some(CheckedArchive {
        _marker: PhantomData,
        bytes,
    }))
}

impl<T> Decoder for RkyvDecoder<T>
where
    T: Archive,
    Archived<T>: for<'a> CheckBytes<HighValidator<'a, RancorError>>,
{
    type Item = CheckedArchive<T>;
    type Error = Error;

    fn decode(&mut self, src: &mut BytesMut) -> Result<Option<Self::Item>, Self::Error> {
        decode_owned(&mut self.inner, src)
    }
}

impl<T> Decoder for RkyvCodec<T>
where
    T: Archive,
    Archived<T>: for<'a> CheckBytes<HighValidator<'a, RancorError>>,
{
    type Item = CheckedArchive<T>;
    type Error = Error;

    fn decode(&mut self, src: &mut BytesMut) -> Result<Option<Self::Item>, Self::Error> {
        decode_owned(&mut self.inner, src)
    }
}

impl<T> Encoder<&T> for RkyvCodec<T>
where
    T: for<'a> Serialize<HighSerializer<'a, Vec<u8>, ArenaHandle<'a>, RancorError>>,
{
    type Error = Error;

    fn encode(&mut self, item: &T, dst: &mut BytesMut) -> Result<(), Self::Error> {
        let writer = rkyv::api::high::to_bytes_in_with_alloc(
            item,
            Vec::with_capacity(size_of::<Archived<T>>()),
            self.arena.acquire(),
        )?;

        self.arena.shrink();

        self.inner.encode(writer.into(), dst).map_err(Error::from)
    }
}

#[cfg(test)]
mod tests {
    use std::{
        collections::HashMap,
        io::{Cursor, Seek},
    };

    use super::*;

    #[derive(rkyv::Archive, rkyv::Serialize, PartialEq, Debug)]
    #[archive(check_bytes)]
    struct Test {
        a: u32,
        b: HashMap<i64, String>,
        c: u128,
    }

    #[test]
    fn test_codec() {
        let data = Test {
            a: 42,
            b: {
                let mut b = HashMap::new();
                b.insert(1, "hello".to_string());
                b.insert(2, "world".to_string());
                b
            },
            c: 1234567890,
        };

        let mut codec = RkyvCodec::<Test>::default();
        let mut buf = BytesMut::new();

        codec.encode(&data, &mut buf).unwrap();

        let decoded = codec.decode(&mut buf).unwrap().unwrap();

        assert_eq!(decoded.a, 42);
        assert_eq!(decoded.b.get(&1.into()).unwrap(), "hello");
        assert_eq!(decoded.b.get(&2.into()).unwrap(), "world");
        assert_eq!(decoded.c, 1234567890);
    }

    #[tokio::test]
    async fn test_framed() {
        use futures_util::{SinkExt, StreamExt};

        let data = Test {
            a: 42,
            b: {
                let mut b = HashMap::new();
                b.insert(1, "hello".to_string());
                b.insert(2, "world".to_string());
                b
            },
            c: 1234567890,
        };

        let mut io = Cursor::new(Vec::new());

        let codec = RkyvCodec::<Test>::default();

        let mut framed = codec.framed(&mut io);

        framed.send(&data).await.unwrap();

        framed.get_mut().rewind().unwrap();

        let decoded = framed.next().await.unwrap().unwrap();

        assert_eq!(decoded.a, 42);
        assert_eq!(decoded.b.get(&1.into()).unwrap(), "hello");
        assert_eq!(decoded.b.get(&2.into()).unwrap(), "world");
        assert_eq!(decoded.c, 1234567890);
    }
}
