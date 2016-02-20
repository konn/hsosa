
#import "Foundation/Foundation.h"

BOOL inline_c_Language_OSA_AppleEventDescriptor_0_e20f668435c58697c00c42f4b07f11aec03f001c(NSAppleEventDescriptor * evt_inline_c_0) {
return ( (evt_inline_c_0).booleanValue );
}


NSData * inline_c_Language_OSA_AppleEventDescriptor_1_331288a4ca5a9f60de3147162bff9373327b250a(NSAppleEventDescriptor * evt_inline_c_0) {
return (evt_inline_c_0.data);
}


int inline_c_Language_OSA_AppleEventDescriptor_2_9da62ad1cf17362296a544e60d1626fb6a37878c(NSAppleEventDescriptor * evt_inline_c_0) {
return ( (evt_inline_c_0).int32Value );
}


NSInteger inline_c_Language_OSA_AppleEventDescriptor_3_1051e0a914033f18b8a2597fd051ce2755312f69(NSAppleEventDescriptor * evt_inline_c_0) {
return ( (evt_inline_c_0).numberOfItems );
}


NSString * inline_c_Language_OSA_AppleEventDescriptor_4_e3101dcc161d8896ac668c10902eceab37a72cec(NSAppleEventDescriptor * evt_inline_c_0) {
return ( (evt_inline_c_0).stringValue );
}


NSAppleEventDescriptor * inline_c_Language_OSA_AppleEventDescriptor_5_3ee380eef71945efb604624a1f56ae6f0cb37464(NSAppleEventDescriptor * evt_inline_c_0, NSInteger n_inline_c_1) {
return ( [(evt_inline_c_0)
                                                      descriptorAtIndex: n_inline_c_1] );
}

