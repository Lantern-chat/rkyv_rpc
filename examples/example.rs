fn main() {}

rkyv_rpc::tuple_enum! {
    pub enum Example: u16 {
        1000 = A(u8),
        1001 = B(u16),
        1002 = C(u32),
    }
}

rkyv_rpc::unit_enum! {
    #[derive(Default)]
    pub enum ExampleUnit: u16 {
        #[default]
        1000 = A,
        1001 = B,
        1002 = C,
    }
}

rkyv_rpc::unit_enum! {
    #[derive(Default)]
    pub enum ExampleByte: u8 {
        #[default]
        100 = A,
        101 = B,
        102 = C,
    }
}

rkyv_rpc::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct ExampleBitflags: u16 {
        const A = 0;
        const B = 1;
        const C = 2;
    }
}

rkyv_rpc::enum_codes! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum ExampleCodes: u16 {
        1000 = A,
        1001 = B,
        1002 = C,
    }
}
