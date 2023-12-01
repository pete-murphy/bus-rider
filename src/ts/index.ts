const url =
  "https://api-v3.mbta.com/predictions/?filter[route]=9&filter[stop]=place-43&stop_sequence=8";
const apiKey = process.env["API_KEY"]!;

const headers = {
  Accept: "text/event-stream",
  "X-API-Key": apiKey,
};

let count = 0;

fetch(url, { headers }).then((response) => {
  const reader = response.body!.getReader();
  const decoder = new TextDecoder();

  async function processResult() {
    const result = await reader.read();
    count++;
    if (result.done || count > 500) {
      return;
    }
    const chunk = decoder.decode(result.value, { stream: true });

    // This works, use this
    // console.log(chunk.split(/event: (\w+)\ndata: (.*)\n\n/));
    console.log(chunk);
    console.log("-".repeat(80));
    console.log("");

    return processResult();
  }

  processResult().catch(console.error);
});

// function processChunk(chunk: string) {
//   if (chunk.startsWith("event: reset")) {
//     console.log("Reset");
//     processData(chunk);
//   }
//   if (chunk.startsWith("event: update")) {
//     console.log("Update");
//     processData(chunk);
//   }
// }

// function processData(chunk: string) {
//   const data = chunk.split("\n")[1]?.replace("data: ", "");
//   console.log(JSON.parse(data!));
// }
