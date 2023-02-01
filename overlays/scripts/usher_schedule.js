#!/usr/bin/env node

const { GoogleSpreadsheet } = require('google-spreadsheet');

const documentId = '1Id8hE3At8WkuEM7kxblAdUSXUMfI5Ar8jwPZF0RFpmM';
const doc = new GoogleSpreadsheet(documentId);

(async () => {
  try {
    
  await doc.useServiceAccountAuth({
    client_email: process.env.USHER_BOT_EMAIL ?? "",
    private_key: process.env.USHER_BOT_SECRET ?? "",
  });

  await doc.loadInfo(); // loads document properties and worksheets

  const sheet = doc.sheetsByTitle['Schedule'];
  const nextSunday = new Date();
  nextSunday.setDate(nextSunday.getDate() + 7 - nextSunday.getDay());
  const row = (await sheet.getRows()).find(r => {
    const date = new Date(r.Date);
    return date.getDate() === nextSunday.getDate()
      && date.getMonth() === nextSunday.getMonth();
  });
  if (!row) {
    console.log("error: couln't fin the next sunay");
  } else {
    const message =
      [`Automated Ushering Reminder for ${nextSunday.toLocaleDateString()}`,
      `Greeter: ${row.Greeter}`,
      `Ushers: ${row["Offering 1"]} and ${row["Offering 2"]}`
      ].join('\n');
    console.log(message);
  }
  } catch (err) {
    console.log(err.toString())
  }
})()

