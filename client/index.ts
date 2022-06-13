const watcherId = Array.from(
  window.crypto.getRandomValues(new Uint8Array(8)),
  (dec) => dec.toString(16)
).join("");

class ClientApp {
  tick: number;
  failures: number;
  handler: number | undefined;

  constructor(
    readonly ticksDiv: HTMLElement,
    readonly numPlantsDiv: HTMLElement,
    readonly numWatchersDiv: HTMLElement,
    readonly gardenDiv: HTMLElement
  ) {
    this.tick = 0;
    this.failures = 0;
  }

  startListening(interval: number) {
    this.handler = setInterval(async () => {
      if (this.failures > 50) {
        clearInterval(this.handler);
        document.body.setAttribute("style", "background: lightgrey");
        this.ticksDiv.innerText = "DISCONNECTED";
        const reconnect = document.createElement("button");
        reconnect.innerText = "Reconnect";
        reconnect.onclick = () => {
          this.startListening(interval);
          reconnect.remove();
        };
        this.ticksDiv.append(reconnect);
        return;
      }

      try {
        let resp = await fetch(
          "/api/getUpdates?" +
            new URLSearchParams({ watcherId, tick: this.tick.toString() })
        );
        if (!resp.ok) {
          throw new Error(resp.statusText);
        } else {
          this.failures = 0;

          const json = await resp.json();
          const tick = json.tick;
          if (!Number.isFinite(tick)) {
            throw new Error(`Expected number for "tick", but was '${tick}'`);
          }
          this.tick = tick;
          this.ticksDiv.innerText = `Age: ${tick} ticks`;

          const numPlants = json.numPlants;
          if (!Number.isFinite(numPlants)) {
            throw new Error(
              `Expected number for "numPlants", but was '${numPlants}'`
            );
          }
          this.numPlantsDiv.innerText = `Plants: ${numPlants}`;

          const numWatchers = json.numWatchers;
          if (!Number.isFinite(numWatchers)) {
            throw new Error(
              `Expected number for "numWatchers", but was '${numWatchers}'`
            );
          }
          this.numWatchersDiv.innerText = `Watchers: ${numWatchers}`;

          this.gardenDiv.innerText = json.garden;
        }
      } catch (error) {
        console.error(error);
        this.failures++;
      }
    }, interval);
  }
}

window.onload = () => {
  const app = new ClientApp(
    document.getElementById("age")!,
    document.getElementById("numPlants")!,
    document.getElementById("numWatchers")!,
    document.getElementById("garden")!
  );
  app.startListening(125);
};
