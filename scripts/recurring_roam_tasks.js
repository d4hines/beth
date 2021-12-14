const RoamPrivateApi = require("d4hines-roam-research-private-api");
const moment = require("moment");

const query = window.roamAlphaAPI.q;

const hasReference = (date, blockID) => {
  let ancestorRule = `[ 
  [ (ancestor ?b ?a) 
       [?a :block/children ?b] ] 
  [ (ancestor ?b ?a) 
       [?parent :block/children ?b ] 
       (ancestor ?parent ?a) ] ] ]`;

  let results = query(
    `[ 
                        :find ?referencingBlock ?string
                        :in $ %
                        :where
                            [?page :node/title ${date}] 
                            (ancestor ?block ?page)
                            [?b :block/uid ${blockID}]
                            [?referencingBlock :block/refs ?b]
                            [?referencingBlock :block/string ?string ]

                        ]`,
    ancestorRule
  );
  return results.length > 0;
};

const getAllRecurringTasksForTag = (tag) => {
  query(
    `[ 
                        :find ?uid
                        :in $ %
                        :where
                            [?page :node/title "${tag}"]
                            [?page :block/uid ?uid]

                        ]`,
    ancestorRule
  );
};

const makeTodo = (blockID, date) => {};

const reconcileTodosForTag = (tag, date) => {
  let tasks = getAllRecurringTasksForTag(tag);
  for (const task of tasks) {
    if (!hasReference(date, task)) {
      makeTodo(task, date);
    }
  }
};

let format = (moment) => moment.format("MMMM Do, YYYY");

let isEven = (x) => x % 2 === 0;

const reconcileTodos = () => {
  let date = moment();
  // Annually
  reconcileTodos("annually", format(date.startOf("year")));
  // Biannually
  if (isEven(date.years())) {
    reconcileTodos("biannually", format(date.startOf("year")));
  }
  // Weekly
  // TODO: currently no weekly tasks
  // reconcileTodos("weekly", format(date.startOf("week")));
  // Biweekly
  if (isEven(date.week())) {
    reconcileTodos("biweekly", format(date.startOf("weekly")));
  }
  //Monthly
  reconcileTodos("monthly", format(date.startOf("month")));
  // Spring
  if (date.month() === 3) {
    reconcileTodos("spring", format(date.startOf("month")));
  }
  // Summer
  if (date.month() === 6) {
    reconcileTodos("summer", format(date.startOf("month")));
  }
  // Fall
  if (date.month() === 9) {
    reconcileTodos("fall", format(date.startOf("month")));
  }
  // Winter
  if (date.month() === 12) {
    reconcileTodos("winter", format(date.startOf("month")));
  }
};

const [graph, email, password] = Buffer.from(
  process.env.ROAM_CREDENTIALS,
  "base64"
)
  .toString("ascii")
  .trim()
  .split("\n");

const api = new RoamPrivateApi(graph, email, password);

(async () => {
  await api.logIn();
  return api.close();
})();
