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
    util::AlignedVec,
    validation::{archive::ArchiveValidator, shared::SharedValidator, Validator},
    Archive, Archived, Serialize,
};

/// Zero-cost wrapper around an rkyv archive that has been checked for validity.
pub struct CheckedArchive<T> {
    _marker: std::marker::PhantomData<T>,
    aligned: rkyv::util::AlignedVec,
    pos: usize,
}

impl<T> Deref for CheckedArchive<T>
where
    T: Archive,
{
    type Target = Archived<T>;

    fn deref(&self) -> &Self::Target {
        // SAFETY: This is safe because the buffer is aligned and the root type
        // was checked when when it was decoded.
        unsafe { rkyv::api::access_pos_unchecked(&self.aligned, self.pos) }
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

fn decode<T>(
    inner: &mut LengthDelimitedCodec,
    src: &mut BytesMut,
) -> Result<Option<CheckedArchive<T>>, Error>
where
    T: Archive,
    Archived<T>: for<'a> CheckBytes<HighValidator<'a, RancorError>>,
{
    let Some(bytes) = inner.decode(src)? else {
        return Ok(None);
    };

    // copy over bytes to ensure alignment
    let mut aligned = AlignedVec::with_capacity(bytes.len());
    aligned.extend_from_slice(&bytes);

    let pos = rkyv::api::root_position::<Archived<T>>(aligned.len());

    // rkyv::access but without the actual access
    rkyv::api::check_pos_with_context::<Archived<T>, _, RancorError>(
        &aligned,
        pos,
        &mut Validator::new(ArchiveValidator::new(&aligned), SharedValidator::new()),
    )?;

    Ok(Some(CheckedArchive {
        _marker: PhantomData,
        aligned,
        pos,
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
        decode(&mut self.inner, src)
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
        decode(&mut self.inner, src)
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
