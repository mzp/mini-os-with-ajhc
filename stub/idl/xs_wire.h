struct xenstore_domain_interface {
  char[] req;
  char[] rsp;
  uint32_t req_cons;
  uint32_t req_prod;
  uint32_t rsp_cons;
  uint32_t rsp_prod;
};

struct xsd_sockmsg {
  uint32_t type;
  uint32_t req_id;
  uint32_t tx_id;
  uint32_t len;
};
