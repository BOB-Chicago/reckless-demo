const init = async () => {
  const node = document.getElementById("app");

  const keychain = localKeychain();
  const startPage = keychain.exists() ? "start" : "keychain.start";

  const state = {
    page: startPage,
    items: new Map(),
    surveys: new Map(),
    cart: new Map(),
    keychain,
    paymentRequests: []
  };

  const handler = async (msg, send) => {
    console.log("PUSH MESSAGE", msg);
    switch (msg.tag) {
      case "object": {
        resolve(state, msg.payload[0], msg.payload[1]);
        break;
      }

      case "confirmation": {
        const getReply = async () => {
          switch (msg.id.type) {
            case "OrderT": {
              const key = await state.keychain.deriveKey("/order");
              return send({
                tag: "resolve",
                id: msg.id.value,
                type: "order",
                key
              });
            }

            case "ItemT": {
              const key = await state.keychain.deriveKey("/item");
              return send({
                tag: "resolve",
                id: msg.id.value,
                type: "item",
                key
              });
            }
            case "SurveyT": {
              const key = await state.keychain.deriveKey("/survey");
              return send({
                tag: "resolve",
                id: msg.id.value,
                type: "survey",
                key
              });
            }
            case "ContributionT": {
              return send({
                tag: "resolve",
                id: msg.id.value,
                type: "contribution"
              });
              break;
            }
            case "DOpT": {
              return null;
              break;
            }
          }
        };

        const reply = await getReply();

        if (reply !== null && reply.tag === "object") {
          resolve(state, msg.id, reply.payload);
        }

        break;
      }
    }
  };

  const ws = BOBSocket(window.bobSocketUrl, handler);
  const update = updater(state);

  if (keychain.exists()) {
    const key = await keychain.deriveKey("/id");
    await ws.send({
      tag: "sync",
      key
    });
  }

  while (true) {
    const us = await new Promise(f => refresh(node, state, ws.send, f));
    update(us);
  }
};

init();
