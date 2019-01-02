const updater = state => async us => {
  us.forEach(u => {
    console.log("UPDATE", u.tag, u.val);
    switch (u.tag) {
      case "nav": {
        state.page = u.val;
        break;
      }
      case "paymentRequest": {
        state.paymentRequests.push(u.val);
        break;
      }
      case "addItemToCart": {
        const inCart = state.cart.has(u.val) ? state.cart.get(u.val) : 0;
        state.cart.set(u.val, inCart + 1);
        break;
      }
      case "removeItem": {
        state.cart.delete(u.val);
        break;
      }
      case "flushCart": {
        state.cart = new Map();
        break;
      }
      case "focusSurvey": {
        state.surveyFocus = u.val;
        break;
      }
      case "newSeed": {
        state.keychain.setSeed(u.val);
        break;
      }
      case "randomSeed": {
        state.keychain.randomSeed();
        break;
      }
      case "clearSeed": {
        state.keychain.clearSeed();
        break;
      }
      case "confirmation": {
        switch (u.val.id.type) {
          case "ItemT": {
            // New item
            break;
          }
          case "SurveyT": {
            // New survey
            break;
          }
          case "OrderT": {
            // New order
            break;
          }
        }
        break;
      }
      case "save": {
        console.log("save not implemented");
        break;
      }
    }
  });
};

const refresh = (node, state, send, updates) => {
  console.log("REFRESH", state);
  const app = {
    send,
    key: state.keychain.deriveKey,
    page: xs => page(node, xs),
    updates,
    error: msg => alert(msg)
  };

  switch (state.page) {
    case "start": {
      startPage(app);
      break;
    }
    case "donations": {
      donationPage(app);
      break;
    }
    case "paymentRequests": {
      paymentRequests(app, state.paymentRequests);
      break;
    }
    case "newItem": {
      newItem(app);
      break;
    }
    case "store": {
      store(
        app,
        Array.from(state.items).map(x => ({
          id: x[0],
          desc: x[1].itemDescription,
          price: x[1].itemPrice,
          inCart: state.cart.get(x[0]) || 0
        }))
      );
      break;
    }
    case "orderConfirmation": {
      if (state.cart.size == 0) {
        app.updates([{ tag: "nav", val: "store" }]);
        return;
      }
      const order = Array.from(state.items)
        .filter(x => state.cart.has(x[0]))
        .map(x => ({
          id: x[0],
          desc: x[1].itemDescription,
          price: x[1].itemPrice,
          quantity: state.cart.get(x[0])
        }));
      orderSummary(app, order);
      break;
    }
    case "newSurvey": {
      newSurvey(app);
      break;
    }
    case "surveys": {
      surveys(
        app,
        Array.from(state.surveys).map(x => ({
          id: x[0],
          title: x[1].surveyTitle,
          questions: x[1].surveyQuestions
        }))
      );
      break;
    }
    case "takeSurvey": {
      if (state.activeSurvey !== null) {
        const id = state.surveyFocus;
        const survey = {
          id,
          title: state.surveys.get(id).surveyTitle,
          questions: state.surveys.get(id).surveyQuestions
        };
        takeSurvey(app, survey);
      }
      break;
    }
    case "keychain.start": {
      keyManagement.start(app, state.keychain.exists());
      break;
    }
    case "keychain.enter": {
      keyManagement.enterSeed(app);
      break;
    }
    case "keychain.show": {
      keyManagement.showSeed(app, state.keychain.getSeed());
      break;
    }
  }
};

const resolve = (state, id, obj) => {
  switch (id.type) {
    case "ItemT": {
      state.items.set(id.value, obj);
      break;
    }
    case "SurveyT": {
      state.surveys.set(id.value, obj);
      break;
    }
    case "ContributionT": {
      break;
    }
    case "DOpT": {
      const h = id.value;
      // Prune from the list of requests
      break;
    }
  }
};

const handlePaymentRequest = (app, msg, desc, additionalUpdates) => {
  if (msg.tag !== "paymentRequest") {
    app.updates([{ tag: "nav", val: "start" }]);
    return;
  }

  app.updates(
    [
      {
        tag: "paymentRequest",
        val: {
          desc,
          date: new Date(),
          hash: msg.rHash,
          req: msg.paymentRequest
        }
      }
    ].concat(additionalUpdates)
  );
};
