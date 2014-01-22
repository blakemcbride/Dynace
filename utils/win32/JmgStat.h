#pragma once

#if defined( __cplusplus )
/* Hint: this namespace is defined to be globally unique.
 * For ease of use, alias it to an easy-to-type name
 * by putting a line such as
 * namespace jmg = 
 *      Jonathan_M_Gilligan_95724E90_4A88_11d5_80F3_006008C7B14D;
 * in your code after #including this file. Then refer to the 
 * namespace as jmg, e.g., jmg::GetFileModTime();
 */
namespace Jonathan_M_Gilligan_95724E90_4A88_11d5_80F3_006008C7B14D  {
extern "C" {
#endif

extern BOOL GetUTCFileModTime ( LPCTSTR name, time_t * utc_mod_time );

#if defined( __cplusplus )
}   // extern "C"
}   // namespace Jonathan_M_Gilligan_95724E90_4A88_11d5_80F3_006008C7B14D
#endif