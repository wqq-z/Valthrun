//! Manual defined offsets which can not be deducted by the CS2 schema.

pub mod schemasystem {
    pub const SYSTEM_SCOPE_SIZE: u64 = 0x190;
    pub const SYSTEM_SCOPE_ARRAY: u64 = SYSTEM_SCOPE_SIZE + 0x08;

    pub const SCOPE_CLASS_BINDINGS: u64 = 0x558;
    
    #[allow(unused)]
    pub const SCOPE_ENUM_BINDINGS: u64 = 0x2DA0;
}

pub mod client {
    // Sig source: https://www.unknowncheats.me/forum/3725362-post1.html
    // https://www.unknowncheats.me/forum/3713485-post262.html
    #[allow(non_snake_case)]
    pub mod CModel {
        /* 85 D2 78 16 3B 91. Offset is array of u32 */
        pub const BONE_FLAGS: u64 = 0x1A8;

        /* 85 D2 78 25 3B 91. Offset is array of *const i8 */
        pub const BONE_NAME: u64 = 0x160;

        /* UC sig does not work. Offset is array of u16 */
        pub const BONE_PARENT: u64 = 0x178;
    }

    #[allow(non_snake_case)]
    pub mod CModelState {
        /* Offset is array of BoneData */
        pub const BONE_STATE_DATA: u64 = 0x80;
    }
}