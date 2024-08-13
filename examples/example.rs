fn main() {}

rkyv_rpc::rpc_enum! {
    pub enum Example: u16 {
        0 = A(u8),
        1 = B(u16),
        2 = C(u32),
    }
}
