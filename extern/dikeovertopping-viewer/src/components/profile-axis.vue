<template>
  <g>
    <g
      v-for="y in steps.y"
      :key="`tick-y-${y}`"
    >
      <line
        :x1="bounds.xMin"
        :x2="bounds.xMax"
        :y1="y"
        :y2="y"
        :stroke-width="scale * 2"
        class="profile-axis__tick"
        stroke-dasharray="2 1"
      />
      <text
        :style="`font-size: ${scale}rem;`"
        :x="bounds.xMin - 2"
        :y="-1 * y + 0.6"
        transform="scale(1, -1)"
        text-anchor="end"
        class="profile-axis__value"
      >
        {{ y }}
      </text>
    </g>
    <g
      v-for="x in steps.x"
      :key="`tick-x-${x}`"
    >
      <text
        :style="`font-size: ${scale}rem;`"
        :x="x + 2"
        :y="-1 * bounds.yMin + 2.8"
        transform="scale(1, -1)"
        text-anchor="end"
        class="profile-axis__value"
      >
        {{ x }}
      </text>
    </g>
    <text
      :style="`font-size: ${scale}rem;`"
      :x="bounds.xMin - 6"
      :y="-1 * bounds.yMax - 2"
      transform="scale(1, -1)"
      class="profile-axis__unit"
    >
      (m+NAP)
    </text>
    <text
      :style="`font-size: ${scale}rem;`"
      :x="bounds.xMax + 1"
      :y="-1 * bounds.yMin + 0.6"
      transform="scale(1, -1)"
      class="profile-axis__unit"
    >
      (m)
    </text>
    <line
      :x1="bounds.xMin"
      :x2="bounds.xMin"
      :y1="bounds.yMin"
      :y2="bounds.yMax"
      :stroke-width="scale * 2"
      class="profile-axis"
    />
  </g>
</template>

<script>
  const range = (start, end) =>
    Array.from({length: (end - start)}, (value, key) => key + start);

  const getNearestTickJump = wantedTickJumps => tickJump => wantedTickJumps
    .reduce((nearestTick, tick) => (
      Math.round(tickJump / tick) === 1
      ? tick
      : nearestTick
    ), 0);

  export default {
    props: {
      bounds: {
        type: Object,
        default() { return {
            xMin: 0,
            xMax: 0,
            yMin: 0,
            yMax: 0
          };
        }
      },
      scale: {
        type: Number,
        default: 1
      },
      tickAmount: {
        type: Number,
        default: 5,
      },
      wantedTickJumps: {
        type: Array,
        default() { return [1, 2, 5, 10, 20, 50, 100, 200, 500]; },
      },
    },
    computed: {
      steps () {
        const getWantedTickJumps = getNearestTickJump(this.wantedTickJumps);

        const steps = {
          y: range(this.bounds.yMin, this.bounds.yMax),
          x: range(this.bounds.xMin, this.bounds.xMax),
        };
        const tickJump = {
          y: Math.round(steps.y.length / this.tickAmount),
          x: Math.round(steps.x.length / this.tickAmount),
        };

        return {
          y: steps.y.filter((step, index) =>
            index % getWantedTickJumps(tickJump.y) === 0
          ),
          x: steps.x.filter((step, index) =>
            index % getWantedTickJumps(tickJump.x) === 0
          ),
        };
      },
    },
  };
</script>

<style>
  .profile-axis {
    stroke: var(--neutral-6);
  }

  .profile-axis__tick {
    stroke: var(--neutral-3);
  }

  .profile-axis__value {
    font-size: 0.2em;
    text-align: right;
    width: 4rem;
    display: block;
  }

  .profile-axis__unit {
    fill: var(--neutral-7);
  }
</style>
