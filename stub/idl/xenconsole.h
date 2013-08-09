struct dom_u {
  uint64_t mfn;
  uint32_t evtchn;
};

struct xencons_interface {
};
struct xenbus_event {
};

struct consfront_dev {
  domid_t dom;
  struct xencons_interface* ring;
  grant_ref_t ring_ref;
  evtchn_port_t evtchn;
  char* nodename;
  char* backend;
  struct xenbus_event* events;
};
