class ClientApp {
  tick: number;
  failures: number;
  handler: number | undefined;

  constructor(readonly ticksDiv: () => HTMLElement) {
    this.tick = 0;
    this.failures = 0;
  }

  startListening(interval: number) {
    this.handler = setInterval(async () => {
      const ticksDiv = this.ticksDiv();
      if (this.failures > 50) {
        clearInterval(this.handler);
        document.body.setAttribute("style", "background: lightgrey");
        ticksDiv.innerText = "DISCONNECTED";
        return;
      }

      try {
        let resp = await fetch(
          "/api/getUpdates?" +
            new URLSearchParams({ tick: this.tick.toString() })
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
          ticksDiv.innerText = `Age: ${tick} ticks`;
        }
      } catch (error) {
        console.error(error);
        this.failures++;
      }
    }, interval);
  }
}

const app = new ClientApp(() => document.getElementById("age")!);
app.startListening(125);
