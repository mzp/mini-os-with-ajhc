struct xenbus_event {
    char* path;
    char* token;
    struct xenbus_event* next;
};

struct watch {
    char* token;
    struct xenbus_event** events;
    struct watch* next;
};

struct xenbus_req_info {
    int in_use;
   void* reply;
};

