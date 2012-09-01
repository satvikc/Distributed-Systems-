/*
 * Please do not edit this file.
 * It was generated using rpcgen.
 */

#ifndef _SIMP_H_RPCGEN
#define _SIMP_H_RPCGEN

#include <rpc/rpc.h>


#ifdef __cplusplus
extern "C" {
#endif


struct operands {
	int x;
};
typedef struct operands operands;

#define SIMP_PROG 0x39876543
#define SIMP_VERSION 1

#if defined(__STDC__) || defined(__cplusplus)
#define NEXT_PRIME 1
extern  int * next_prime_1(operands *, CLIENT *);
extern  int * next_prime_1_svc(operands *, struct svc_req *);
extern int simp_prog_1_freeresult (SVCXPRT *, xdrproc_t, caddr_t);

#else /* K&R C */
#define NEXT_PRIME 1
extern  int * next_prime_1();
extern  int * next_prime_1_svc();
extern int simp_prog_1_freeresult ();
#endif /* K&R C */

/* the xdr functions */

#if defined(__STDC__) || defined(__cplusplus)
extern  bool_t xdr_operands (XDR *, operands*);

#else /* K&R C */
extern bool_t xdr_operands ();

#endif /* K&R C */

#ifdef __cplusplus
}
#endif

#endif /* !_SIMP_H_RPCGEN */