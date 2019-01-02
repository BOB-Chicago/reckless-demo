/* Page text */

const content = {
  connectNode: `Connect to our LND node at ${window.nodeUri}`,
  startText:
    "This app provides some free functionality and some pay functionality.  First, you can donate to BOB Chicago.  (We appreciate every satoshi!)  You can also (paid) create a survey or (free) take a survey.  Finally, you can order items from our store.",
  paymentRequests:
    "In the lightning network, you need to get some cryptographic data from the person you are paying before you can make the payment.  In the future, most of this will be handled behind the scenes.  However since it is early, you'll need to copy the URI for the payment you want to make below into your lightning wallet.",
  keys:
    "There is no stored keychain available.  To use this app, you generate a master key and use it for all your activity.  You can generate a random one now, or enter one that you've saved."
};

const navButton = (app, dest, text) => {
  return button(text, () => {
    app.updates([{ tag: "nav", val: dest }]);
  });
};

const mainPage = app => navButton(app, "start", ">> start page");

/* start */

const startPage = async app => {
  
  // Backup data to the server
  const backup = button("backup your data", async () => {
    app.updates([
      { tag: "backup", val: null },
      { tag: "nav", val: "paymentRequests" }
    ]);
  });

  const restore = button("restore your data", async app => {
    // Not implemented
    app.updates([]);
  });

  const always = [navButton(app, "keychain.start", "manage keys")];

  const testKey = await app.key("/start");

  const options =
    testKey === null
      ? always
      : always.concat([
          navButton(app, "paymentRequests", "view payment requests"),
          navButton(app, "surveys", "take a survey"),
          navButton(app, "donations", "make a donation"),
          navButton(app, "store", "visit the store"),
          navButton(app, "newSurvey", "add a survey"),
          navButton(app, "newItem", "add an item"),
          backup,
          restore
        ]);

  app.page([
    big("Welcome!"),
    row([]),
    row(options),
    explainer(content.startText),
    explainer(content.connectNode)
  ]);
};

/* donation */

const donationPage = app => {
  const satoshis = input();
  const message = input();

  const getRequest = button("get payment request", async () => {
    const payload = {
      tag: "donate",
      message: message.value,
      amount: parseInt(satoshis.value)
    };

    const result = await app.send(payload);
    handlePaymentRequest(app, result, `donation: "${message.value}"`, [
      { tag: "nav", val: "paymentRequests" }
    ]);
  });

  app.page([
    big("#reckless BOB donation"),
    block([
      par("Enter donation amount in satoshis:"),
      satoshis,
      par("Enter your donation message:"),
      message
    ]),
    row([getRequest, mainPage(app)])
  ]);
};

/* payment request */

const paymentRequests = (app, prs) => {
  app.page([
    big("Your payment requests:"),
    explainer(content.paymentRequests),
    block(prs.map(pr => p([formatRequest(pr)]))),
    mainPage(app)
  ]);
};

/* orders, items, & store */

const newItem = app => {
  const desc = input();
  const price = input();
  const submit = button("Submit", async () => {
    const key = await app.key("/item");
    const result = await app.send({
      tag: "newItem",
      key,
      description: desc.value,
      price: parseInt(price.value)
    });

    const navStart = { tag: "nav", val: "start" };
    switch (result.tag) {
      case "ack": {
        app.updates([navStart]);
        break;
      }
      case "confirmation": {
        app.updates([{ tag: "confirmation", val: result }, navStart]);
        break;
      }
    }
  });

  app.page([
    big("Add a new item"),
    block([par("Item description"), desc, par("Price"), price]),
    row([submit, mainPage(app)])
  ]);
};

const store = (app, items) => {
  const addToCart = id =>
    button("add to cart", () =>
      app.updates([{ tag: "addItemToCart", val: id }])
    );

  const removeFromCart = id =>
    button("remove", () => app.updates([{ tag: "removeItem", val: id }]));

  const submit = navButton(app, "orderConfirmation", "Checkout");

  const cancel = button("Cancel", () =>
    app.updates([{ tag: "flushCart", val: null }, { tag: "nav", val: "start" }])
  );

  app.page([
    big("Welcome to the store!"),
    block(
      items.map(item =>
        block([
          par(formatItem(item)),
          row([addToCart(item.id), removeFromCart(item.id)])
        ])
      )
    ),
    row([submit, cancel, mainPage(app)])
  ]);
};

const orderSummary = (app, order) => {
  const submit = button("Submit order", async () => {
    const key = await app.key("/order");
    const result = await app.send({
      tag: "purchase",
      key,
      stuff: order.map(item => [item.id, item.quantity])
    });

    handlePaymentRequest(app, result, "order", [
      { tag: "flushCart", val: null },
      { tag: "nav", val: "paymentRequests" }
    ]);
  });

  const back = navButton(app, "store", "Back to the store");
  const cancel = button("Cancel order", () =>
    app.updates([{ tag: "flushCart", val: null }, { tag: "nav", val: "start" }])
  );

  app.page([
    big("Order summary"),
    block(order.map(entry => par(formatOrderEntry(entry)))),
    row([submit, back, cancel])
  ]);
};

/* survey */

const newSurvey = app => {
  const title = input();

  const questions = [];
  const questionBlock = block([]);

  const addQuestion = () => {
    const question = input();
    questions.push(question);
    questionBlock.appendChild(block([question]));
  };

  addQuestion();

  const submit = button("submit survey", async () => {
    const key = await app.key("/survey");

    const result = await app.send({
      tag: "newSurvey",
      key,
      title: title.value,
      questions: questions.map(q => q.value)
    });

    handlePaymentRequest(app, result, `new survey: ${title.value}`, [
      { tag: "nav", val: "paymentRequests" }
    ]);
  });

  app.page([
    big("New survey"),
    block([par("Survey title"), title]),
    par("Survey questions"),
    questionBlock,
    row([button("add question", addQuestion), submit, mainPage(app)])
  ]);
};

const surveys = (app, surveys) => {
  const surveyButton = survey =>
    button(survey.title, () =>
      app.updates([
        { tag: "focusSurvey", val: survey.id },
        { tag: "nav", val: "takeSurvey" }
      ])
    );

  app.page([
    big("Surveys"),
    row(surveys.map(survey => surveyButton(survey))),
    row([mainPage(app)])
  ]);
};

const takeSurvey = (app, survey) => {
  const answers = [];
  const surveyBlock = block(
    survey.questions.map(q => {
      const answer = input();
      answers.push(answer);
      return p([normal(q), answer]);
    })
  );

  const submit = button("submit responses", () => {
    app.send({
      tag: "surveyResponse",
      id: survey.id,
      responses: answers.map(a => a.value)
    });
    app.updates([{ tag: "nav", val: "surveys" }]);
  });

  app.page([
    big(`Survey: ${survey.title}`),
    surveyBlock,
    row([
      submit,
      navButton(app, "surveys", "return to survey list"),
      mainPage(app)
    ])
  ]);
};

/* key management */

const keyManagement = {
  start: (app, exists) => {
    const banner = exists ? block([]) : explainer(content.keys);

    controls = () => {
      if (exists) {
        const show = button("show seed", () =>
          app.updates([{ tag: "nav", val: "keychain.show" }])
        );

        const flush = button("forget seed", () =>
          app.updates([{ tag: "clearSeed", val: null }])
        );

        return row([show, flush, mainPage(app)]);
      } else {
        const enter = button("enter a seed", () =>
          app.updates([{ tag: "nav", val: "keychain.enter" }])
        );

        const random = button("use a random seed", () =>
          app.updates([{ tag: "randomSeed", val: null }])
        );

        return row([enter, random, mainPage(app)]);
      }
    };

    app.page([big("Keychain management"), banner, controls()]);
  },

  enterSeed: app => {
    const seedEntry = input();

    const submit = button("submit seed", () => {
      if (isValidHex(seedEntry.value) && seedEntry.value.length === 64) {
        app.updates([
          { tag: "newSeed", val: hexDecode(seedEntry.value) },
          { tag: "nav", val: "keychain.start" }
        ]);
      } else {
        app.error(
          "Invalid seed!  Your seed should be 32 bytes encoded in hex."
        );
      }
    });

    app.page([
      big("Key management"),
      block([par("enter seed in hexadecimal format"), seedEntry]),
      row([submit, mainPage(app)])
    ]);
  },

  showSeed: (app, seed) => {
    const done = button("done", () =>
      app.updates([{ tag: "nav", val: "keychain.start" }])
    );

    app.page([
      big("Key management"),
      par(hexEncode(Array.from(seed))),
      row([done, mainPage(app)])
    ]);
  }
};

const storage = app => {
  app.page([
    big("Pay to store a JSON document"),
    explainer(content.storage),
    row([blobInput]),
    row([submit, mainPage(app)])
  ]);
};
