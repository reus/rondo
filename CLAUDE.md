# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Rondo is a positional football (soccer) simulation built with ClojureScript. A "rondo" is a Dutch football training drill where teams practice ball possession in a confined space. The project simulates player movement, ball physics, and collision detection on an HTML5 canvas.

## Build & Development Commands

**Hot-reload development (primary workflow):**
```bash
clj -M:fig
```
Starts Figwheel on port 9500, watches `src/cljs` and `src/cljs_worker`, auto-rebuilds on changes.

**Build without watching:**
```bash
clj -M:build-cljs      # Build main app → target/public/cljs-out/dev-main.js
clj -M:build-worker    # Build Web Worker → target/public/cljs-out/dev_worker-main.js
```

**Run Clojure HTTP server (port 3000):**
```bash
clj -M:run-clj
```

**Start nREPL (port 7888, CIDER-compatible):**
```bash
clj -M:nrepl
```

There are no tests in this project.

## Architecture

### Data Flow
Unidirectional: User input → Game loop (core.async channels) → State update (pure functions) → Canvas render

### Key Modules (all under `src/cljs/dev/reus/rondo/`)

- **gameloop.cljs** — Central orchestrator. Runs an async game loop that multiplexes three channels (refresh timer, UI events, Web Worker messages) with priority ordering. Maintains the immutable game state hashmap and drives the step→render cycle.
- **model.cljs** — Pure game logic: Newtonian physics (acceleration→velocity→position with friction), player-player and player-ball collision detection/response using vector projections, ball state machine, and AI behaviors (approach ball, move to destination).
- **gamedata.cljs** — Static configuration: pitch dimensions (400×400), player/ball radii, physics constants, team definitions (Germany/Holland), initial player positions.
- **ui.cljs** — Reagent components and input handling. Manages a global Reagent atom for UI state. Keyboard (arrow keys, Z for shoot, Shift/Ctrl modifiers) and mouse events are sent to the game loop via core.async channels.
- **canvas2d.cljs** — 2D canvas rendering: pitch, players as colored circles with direction indicators, ball, selection highlights.
- **math.cljs** — Vector math library: dot product, magnitude, normalize, projection, rotation, circle overlap detection. All 2D vectors represented as `[x y]` tuples.

### Server (`src/clj/dev/reus/rondo/core.clj`)
Minimal Compojure/Ring server that serves `index.html` and static files.

### Web Worker (`src/cljs_worker/dev/reus/rondo_worker/core.cljs`)
Stub implementation for offloading computation. Communicates via Uint16Array messages. Placeholder for future ML integration.

### Game State Structure
The game state is an immutable hashmap threaded through pure functions. Key parts:
- `:players` — Vector of player maps with `:pos`, `:velocity`, `:acceleration`, `:direction`, `:goal` (behavior state machine), `:team`
- `:ball` — Map with `:state` (`:with-player`, `:shooting`, `:moving`, `:still`, etc.), `:pos`, `:velocity`, `:ke` (kinetic energy)
- `:teams`, `:refresh-rate`, `:frame`, `:time`, `:frame-time`

### Conventions
- Namespaces: `dev.reus.rondo.*`
- Functions with `!` suffix have side effects (e.g., `render!`, `start-server!`)
- Colors: RGB as `[r g b]` (0–1 range), converted to CSS strings for rendering
- Physics uses delta-time integration for frame-rate independence

## LLM-Assisted AI Training (Prototype)

### Concept
Use a local LLM (via Ollama) to help design and iterate on player AI — not by calling the LLM at runtime during gameplay, but by using it offline to generate and refine reward functions and behavior rules.

### Approach: LLM-Generated Reward Functions

The hardest part of training game AI with reinforcement learning is designing good reward functions. The idea is to describe desired behavior in natural language and have the LLM translate that into mathematical reward functions.

**Example workflow:**
1. Describe behavior: "As a defender, always stay on the line between the ball and your own goal"
2. LLM generates a reward function: `reward = -distance(defender.pos, line_segment(ball.pos, own_goal.pos))`
3. Run training episodes in the simulation using that reward function
4. Feed results (metrics, state snapshots) back to the LLM to refine the reward

### Architecture

```
┌─────────────────────────────────────────────────────┐
│  Offline Training Loop                              │
│                                                     │
│  ┌──────────┐    natural language    ┌───────────┐  │
│  │  Human   │ ──────────────────────→│  Ollama   │  │
│  │ (coach)  │    behavior desc.      │  (local   │  │
│  └──────────┘                        │   LLM)    │  │
│                                      └─────┬─────┘  │
│                                            │        │
│                              reward fn / code       │
│                                            │        │
│                                      ┌─────▼─────┐  │
│  ┌──────────┐   state snapshots      │ Training  │  │
│  │  Rondo   │ ◄─────────────────────→│  Runner   │  │
│  │   Sim    │   run episodes         │           │  │
│  └──────────┘                        └─────┬─────┘  │
│                                            │        │
│                              metrics / evaluation   │
│                                            │        │
│                                      ┌─────▼─────┐  │
│                                      │  Ollama   │  │
│                                      │ (refine)  │  │
│                                      └───────────┘  │
└─────────────────────────────────────────────────────┘
```

### Alternative/Complementary Approaches
- **LLM as batch labeler**: Sample game states, have LLM score them (e.g., "is this defender well-positioned? 0-10"), train a small fast reward model from those labels
- **LLM for curriculum design**: Let the LLM design training scenarios and difficulty progression

### Tech Stack for Prototype
- **Ollama** — local LLM inference (free, simple HTTP API at localhost:11434)
- **Model** — Llama 3 8B (runs on 8GB+ RAM) or similar
- **Communication** — HTTP calls from Clojure/ClojureScript to Ollama's REST API
- **Training runner** — Clojure process that runs headless simulation episodes, collects metrics, calls Ollama for reward function generation/refinement

### Key Insight
The LLM is never in the hot loop. It runs offline, producing artifacts (reward functions, behavior rules) that the simulation uses at full speed. This sidesteps the latency and cost problems of using an LLM at runtime.
