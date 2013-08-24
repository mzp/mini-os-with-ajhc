struct thread {
    char* name;
    char* stack;
    uint32_t flags;
    int64_t wakeup_time;
};
