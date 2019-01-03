// ~~~~~~~~~~~~~~~~ //
// Custom WebSocket //
// ~~~~~~~~~~~~~~~~ //

const BOBSocket = (url, handle) => {
  let ws = new WebSocket(url);
  let key = null;
  let buffer = [];
  const callbacks = new Map();

  let nonce = 0;
  const withNonce = f => {
    const m = nonce;
    nonce++;
    f(m);
  };

  const flushBuffer = () => {
    const toSend = Array.from(buffer);
    buffer = [];

    if (toSend.length > 0) {
      console.log(`flushing ${toSend.length} messages`);
    }

    toSend.forEach(x => ws.send(JSON.stringify(x)));
  };

  setInterval(flushBuffer, 1000);

  const send = msg =>
    new Promise((resolve, reject) => {
      withNonce(n => {
        msg.nonce = n;
        console.log("OUTBOUND MESSAGE", msg);

        if (ws.readyState === 1) {
          console.log("sending ...");

          flushBuffer();

          ws.send(JSON.stringify(msg));
          callbacks.set(n, resolve);
        } else {
          console.log("buffering ...");

          buffer.push(msg);
          callbacks.set(n, resolve);
        }
      });
    });

  const onMessage = raw => {
    console.log("WS MESSAGE", raw.data);
    const msg = JSON.parse(raw.data);

    if (msg.hasOwnProperty("ref")) {
      const cb = callbacks.get(msg.ref);
      callbacks.delete(msg.ref);
      if (msg.tag === "confirmation") {
        handle(msg, send);
      }
      cb(msg);
    } else {
      handle(msg, send);
    }
  };

  const setKey = k => {
    key = k;
    send({ tag: "sync", key });
  };

  ws.onmessage = onMessage;

  ws.onclose = () => {
    console.log("CONNECTION CLOSED");

    ws = new WebSocket(url);
    ws.onmessage = onMessage;

    if (key !== null) {
      send({
        tag: "sync",
        key
      });
    }
  };

  return { send, setKey };
};

// ~~~~~~~~~~~ //
// Object keys //
// ~~~~~~~~~~~ //

const hash = bytes => window.crypto.subtle.digest("SHA-256", bytes);

const localKeychain = () => {
  const seedKey = "masterSeed";

  const setSeed = seed =>
    window.localStorage.setItem(seedKey, JSON.stringify(seed));

  const clearSeed = () => window.localStorage.removeItem(seedKey);

  const exists = () => window.localStorage.getItem(seedKey) !== null;

  const randomSeed = () => {
    const bytes = new Uint8Array(32);
    window.crypto.getRandomValues(bytes);
    setSeed(Array.from(bytes));
  };

  const encoder = new TextEncoder();
  const deriveKey = async ref => {
    if (exists()) {
      const refData = encoder.encode(ref);
      const seed = getSeed();
      const data = Uint8Array.from(
        Array.from(seed).concat(Array.from(refData))
      );
      const result = await hash(data);
      return hexEncode(Array.from(new Uint8Array(result)));
    } else {
      return null;
    }
  };

  const getSeed = () =>
    new Uint8Array(JSON.parse(window.localStorage.getItem(seedKey)));

  return {
    setSeed,
    clearSeed,
    exists,
    randomSeed,
    deriveKey,
    getSeed
  };
};

// ~~~~~~~~~~~~~~~ //
// Visual elements //
// ~~~~~~~~~~~~~~~ //

const textNode = text => document.createTextNode(text);

const simple = name => text => {
  const node = document.createElement(name);
  node.setAttribute("class", "simple");
  node.innerText = text;
  return node;
};

const big = simple("h1");
const normal = simple("span");
const par = simple("p");
const a = simple("a");

const mkNode = name => xs => {
  const node = document.createElement(name);
  xs.forEach(x => node.appendChild(x));
  return node;
};

const inline = mkNode("span");
const div = mkNode("div");

const block = xs => {
  const node = div(xs);
  node.setAttribute("class", "column");
  return node;
};

const row = xs => {
  const node = div(xs);
  node.setAttribute("class", "row");
  return node;
};

const p = mkNode("p");

const input = () => {
  const node = document.createElement("input");
  return node;
};

const button = (text, f) => {
  const outer = document.createElement("div");

  const node = document.createElement("span");
  node.setAttribute("class", "button");
  node.innerText = text;
  node.onclick = f;

  outer.appendChild(node);
  return outer;
};

const link = (href, text) => {
  const node = a(text);
  node.setAttribute("href", href);
  return node;
};

const explainer = text => {
  const node = document.createElement("div");
  node.setAttribute("class", "explainer");
  node.innerText = text;
  return node;
};

// ~~~~~~~~~~ //
// Formatting //
// ~~~~~~~~~~ //

const inDollars = cents => "$" + cents / 100;

const formatRequest = pr =>
  inline([
    textNode(`(${pr.date.toString()} ${pr.complete ? "PAID" : ""}): `),
    link("lightning:" + pr.req, pr.desc)
  ]);

const formatItem = item =>
  `${item.desc} @ ${inDollars(item.price)} (x${item.inCart})`;

const formatOrderEntry = orderEntry =>
  `${orderEntry.quantity} / ${orderEntry.desc} @ ${inDollars(
    orderEntry.price * orderEntry.quantity
  )}`;

const surveyBrief = survey => survey.title;

// ~~~~~~~~~~~~~~~~~ //
// Utility functions //
// ~~~~~~~~~~~~~~~~~ //

const clear = node => {
  const xs = Array.from(node.children);
  xs.forEach(x => node.removeChild(x));
  return xs;
};

const page = (node, xs) => {
  clear(node);
  xs.forEach(x => node.appendChild(x));
  return node;
};

// ~~~~~ //
// Silly //
// ~~~~~ //

// prettier-ignore
const hexmap = { 0: 0, 1: 1, 2: 2, 3: 3, 4: 4, 5: 5, 6: 6, 7: 7, 8: 8, 9: 9, a: 10, A: 10, b: 11, B: 11, c: 12, C: 12, d: 13, D: 13, e: 14, E: 14, f: 15, F: 15 };

const hexdigits = "0123456789abcdef";
const hextest = /[0-9a-fA-F]*/;

const isValidHex = hex =>
  hex.length === 2 * Math.floor(hex.length / 2) && hextest.test(hex);

const hexDecode = hex => {
  const result = [];
  for (let i = 0; i < hex.length; i += 2) {
    if (i + 2 > hex.length) {
      return result;
    }

    const b = 16 * hexmap[hex[i]] + hexmap[hex[i + 1]];
    result.push(b);
  }
  return result;
};

const hexEncode = bytes =>
  bytes
    .map(b => {
      const q = Math.floor(b / 16);
      const r = b - 16 * q;
      return [hexdigits[q], hexdigits[r]].join("");
    })
    .join("");
