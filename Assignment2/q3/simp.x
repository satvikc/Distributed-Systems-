#define VERSION_NUMBER 1
struct operands {
    int x;
};
program SIMP_PROG {
    version SIMP_VERSION {
        int NEXT_PRIME(operands) = 1;
        } = 1;
} = 0x39876543;
