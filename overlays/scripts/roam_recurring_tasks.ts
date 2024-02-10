#! /usr/bin/env nix-shell
/*
#! nix-shell -i bun -p bun
*/

import moment from "moment";

if(!process.env.ROAM_TOKEN) {
  console.error("No ROAM_TOKEN provided in environment");
  process.exit(1);
}

async function roamAPI(endpoint: string, data: any) {
  await new Promise((res) => setTimeout(res, 1500));
  const result = Bun.spawnSync({
    cmd: [
      "curl",
      "-X",
      "POST",
      `https://api.roamresearch.com/api/graph/d4hines/${endpoint}`,
      "--location-trusted",
      "-H",
      "accept: application/json",
      "-H",
      `Authorization: Bearer ${process.env.ROAM_TOKEN}`,
      "-H",
      "Content-Type: application/json",
      "-d",
      `${JSON.stringify(data)}`,
    ],
  });
  if (result.success) {
    const text = result.stdout!.toString();
    console.log("debug: api response", text);
    return text;
  } else {
    throw new Error(`${result.stderr}`);
  }
}

export async function q(query: string, args?: string[]): Promise<any> {
  const data = args ? { query, args } : { query };
  const result = await roamAPI("q", data);
  return JSON.parse(result).result;
}

async function createBlock(params: {
  location: { "parent-uid": string; order: number | string };
  block: { uid?: "string"; string: string };
}) {
  const data = { action: "create-block", ...params };
  await roamAPI("write", data);
}

async function deleteBlock(uuid: string) {
  const data = { action: "delete-block", block: { uuid } };
  await roamAPI("write", data);
}

async function updateBlock(block: { uid: string; string: string }) {
  const data = { action: "update-block", block };
  await roamAPI("write", data);
}

async function pull(params: { selector: string; eid: string }) {
  const result = await roamAPI("pull", params);
  return JSON.parse(result).result;
}

let roamFormat = "MMMM Do, YYYY";

function runTag(text: string) {
  const now = new Date();
  switch (true) {
    case text.includes("#weekly"):
      return now.getDay() === 6;
    case text.includes("#biweekly"):
      return now.getDate() === 1 || now.getDate() === 15;
    case text.includes("#monthly"):
      return now.getDate() === 1;
    case text.includes("#bimonthly"):
      return now.getMonth() % 2 === 0 && now.getDate() === 1;
    case text.includes("#biannually"):
      return (
        now.getFullYear() % 2 === 0 &&
        now.getDate() === 1 &&
        now.getMonth() === 0
      );
    case text.includes("#spring"):
      return now.getDate() === 1 && now.getMonth() === 2;
    case text.includes("#fall"):
      return now.getDate() === 1 && now.getMonth() === 8;
    case text.includes("#summer"):
      return now.getDate() === 1 && now.getMonth() === 5;
    case text.includes("#winter"):
      return now.getDate() === 1 && now.getMonth() === 12;
    case text.includes("#yearly"):
      return now.getDate() === 1 && now.getMonth() === 0;
  }
}

const makeTodos = async () => {
  const todaysBlocks = await pull({
    selector: "[:node/title :block/string :block/order {:block/children ...}]",
    eid: `[:node/title "${moment().format(roamFormat)}"]`,
  });
  let references = await q(
    `[
      :find ?uid ?text ?page 
      :where
      [?page :node/title "Recurring Tasks"]
      [?e :block/refs ?page]
      [?e :block/string ?text]
      [?e :block/uid ?uid]
    ]`
  );

  for (const [uid, text] of references) {
    console.log(`Found todo: '${text}'`);
    if (runTag(text)) {
      console.log("It's time to create new todo");
      if (
        todaysBlocks[":block/children"].some((x: any) =>
          x[":block/string"].includes(uid)
        )
      ) {
        console.log("Found this TODO already. Skipping");
        continue;
      }
      const todo_text = `{{[[TODO]]}} ((${uid}))`;
      await createBlock({
        location: {
          "parent-uid": moment().format("MM-DD-YYYY"),
          order: "last",
        },
        block: { string: todo_text },
      });
      console.log("Todo created");
    }
  }
};

async function promoteDeferredTodos() {
  let results = await q(
    `[
      :find ?uid ?text ?other_page_title
      :in $
      :where
      [?page :node/title "Deferred TODO"]
      [?e :block/refs ?page]
      [?e :block/refs ?other_page]
      [?e :block/string ?text]
      [?e :block/uid ?uid]
      [?other_page :node/title ?other_page_title]
      [(!= ?page ?other_page)]
    ]`
  );
  console.log(results);
  for (const [uid, text, dateStr] of results) {
    let date = moment(dateStr, roamFormat);
    const diff = moment().diff(date, "hours");
    if (diff > 0) {
      const updatedText = text.replace("[[Deferred TODO]]", "{{[[TODO]]}}");
      console.log(`promoting todo '${updatedText}'`);
      await updateBlock({ uid, string: updatedText });
    }
  }
}

(async () => {
  console.log("Beginning roam_recurring_tasks");
  while (true) {
    console.log("Making todo's");
    await makeTodos();
    console.log("Promoting deferred todo's");
    await promoteDeferredTodos();
    console.log("done");
    await new Promise((res) => setTimeout(res, 1000 * 60 * 60));
  }
})();
