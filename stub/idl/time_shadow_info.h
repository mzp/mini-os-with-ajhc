struct shadow_time_info {
	uint64_t tsc_timestamp;     /* TSC at last update of time vals.  */
	uint64_t system_timestamp;  /* Time, in nanosecs, since boot.    */
	uint32_t tsc_to_nsec_mul;
	uint32_t tsc_to_usec_mul;
	int tsc_shift;
	uint32_t version;
};

struct vcpu_time_info {
    uint32_t version;
    uint64_t tsc_timestamp;   /* TSC at last update of time vals.  */
    uint64_t system_time;     /* Time, in nanosecs, since boot.    */
    uint32_t tsc_to_system_mul;
    int8_t   tsc_shift;
};

struct shared_info {
    uint32_t wc_version;      /* Version counter: see vcpu_time_info_t. */
    uint32_t wc_sec;          /* Secs  00:00:00 UTC, Jan 1, 1970.  */
    uint32_t wc_nsec;         /* Nsecs 00:00:00 UTC, Jan 1, 1970.  */
};

struct timespec {
    time_t      tv_sec;
    long        tv_nsec;
};
