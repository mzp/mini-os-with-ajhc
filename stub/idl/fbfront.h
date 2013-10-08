struct xenfb_page {
  uint32_t in_cons;
  uint32_t in_prod;
  uint32_t out_cons;
  uint32_t out_prod;
  int32_t width;
  int32_t height;
  uint32_t line_length;
  uint32_t mem_length;
  uint8_t depth;

   uint64_t[] pd;
};

struct fbfront_dev {
  uint16_t dom;

    struct xenfb_page* page;
    uint32_t evtchn;

    char* nodename;
    char* backend;
    int request_update;

    int width;
    int height;
    int depth;
    int stride;
    int mem_length;
    int offset;

    struct xenbus_event* events;
};
