use cs2_schema_cutl::PtrCStr;
use raw_struct::raw_struct;

#[raw_struct(size = 0x290)]
pub struct CNetworkGameClient {
    #[field(offset = 0x200)]
    pub map_path: PtrCStr,

    #[field(offset = 0x208)]
    pub map_name: PtrCStr,
}
