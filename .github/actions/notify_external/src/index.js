const core = require("@actions/core");
const axios = require("axios");

async function run() {
  try {
    const ocBotUrl = core.getInput("oc_bot_url");
    const apiKey = core.getInput("api_key");
    const prTitle = process.env.GITHUB_REF; // Get the PR title
    const prUrl =
      process.env.GITHUB_SERVER_URL +
      "/" +
      process.env.GITHUB_REPOSITORY +
      "/pull/" +
      process.env.GITHUB_REF.split("/")[2];

    const payload = {
      title: prTitle,
      url: prUrl,
    };

    const headers = {
      "X-Auth": apiKey,
      "Content-Type": "application/json",
    };

    await axios.post(ocBotUrl, payload, { headers });
    console.log("Notification sent successfully");
  } catch (error) {
    core.setFailed(`Action failed with error: ${error}`);
  }
}

run();
