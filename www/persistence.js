const userState = () => {
  // master key
  const getKey = () => {
    const raw = window.localStorage.getItem("masterSeed"); 
    return raw === null ? null :
      hexDecode(raw);
  }

  // payment requests
  const getPaymentRequests = () => {
    const raw = window.localStorage.getItem("paymentRequests");
    return raw === null ? [] :
      JSON.parse(raw).map(pr => ({
        desc: pr.desc,
        rHash: pr.rHash,
        req: pr.req,
        date: new Date(pr.date)
      }));
  }
  
  // previous orders
  const getOrderHistory = () => {
    const raw = window.localStorage.getItem("orderHistory");
    return raw === null ? [] :
      JSON.parse(raw);
  }

  return { key: getKey(), paymentRequests: getPaymentRequests(), orderHistory: getOrderHistory() };
}

const storableState = state =>
  JSON.stringify({
    key: hexEncode(state.key),
    orderHistory: state.history,
    paymentRequests: state.paymentRequests
  });

const putUserState = state => {
  const put = (k, v) => 
    window.localStorage.setItem(k, JSON.stringify(v));

  if (state.key !== null) {
    put("key", hexEncode(state.key));
  }

  put("paymentRequests", state.paymentRequests);
  put("orderHistory", state.history);
}
