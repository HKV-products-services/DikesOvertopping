<template>
  <svg
    :viewBox="`${viewBox.xMin} ${viewBox.yMin} ${viewBox.width} ${viewBox.height}`"
    :width="width"
    :height="height"
    class="dike-profile"
    xmlns="http://www.w3.org/2000/svg"
    xmlns:xlink="http://www.w3.org/1999/xlink"
  >
    <profile-water
      :bounds="dikeBoundingBox"
      :water-level="$store.state.load.waterLevel"
      :wave-height="$store.state.load.height"
      :wave-period="$store.state.load.period"
      class="dike-profile__water"
    />
    <polygon
      :points="dikeProfileFill"
      class="dike-profile__background"
    />
    <polyline
      :points="dikeProfileLine"
      :stroke-width="4 * scaleFactor"
      class="dike-profile__line"
    />
    <polyline
      v-if="selection.segment !== undefined"
      :points="selectedLine"
      :stroke-width="4 * scaleFactor"
      class="dike-profile__line--selected"
    />
    <profile-point
      v-for="(point, index) in validatedPoints"
      :key="index"
      :scale="scaleFactor"
      :point="point"
      :selected="(point === selection.point)"
    />
    <line
      v-if="result.success"
      :x1="viewBox.xMin"
      :x2="dikeBoundingBox.xMax"
      :y2="result.dikeHeight"
      :y1="result.dikeHeight"
      :stroke-width="2 * scaleFactor"
      class="dike-profile__result-dike-height"
    />
    <line
      v-if="result.success"
      :x1="viewBox.xMin"
      :x2="dikeBoundingBox.xMax"
      :y2="result.waveRunUp"
      :y1="result.waveRunUp"
      :stroke-width="2 * scaleFactor"
      class="dike-profile__result-discharge"
      stroke-dasharray="0.1 0.6"
      stroke-linecap="round"
      stroke-linejoin="round"
    />
  </svg>
</template>

<script>
  import { mapState } from 'vuex';
  import ProfilePoint from './profile-point.vue';
  import ProfileWater from './profile-water.vue';

  export default {
    components: {
      ProfilePoint,
      ProfileWater,
    },
    props: {
      width: {
        type: Number,
        default: 640
      },
      height: {
        type: Number,
        default: 240
      },
    },
    computed: {
      ...mapState([
        'points',
        'selection',
        'result',
        'calculationMethod',
      ]),
      validatedPoints() {
        return this.points.filter(point =>
          typeof point.x === 'number' &&
          typeof point.y === 'number' &&
          typeof point.roughness === 'number'
        );
      },
      scaleFactor() {
        const { height, width } = (process.client && this.$el)
          ? this.$el.getBoundingClientRect()
          : this;
        const maxMetersPerPixel = Math.max(
          this.viewBox.width / width,
          this.viewBox.height / height
        );

        return maxMetersPerPixel;
      },
      dikeProfileLine() {
        return this.validatedPoints
          .map(({ x, y }) => `${x},${y}`)
          .join(' ');
      },
      dikeProfileFill() {
        return `
          ${this.dikeProfileLine}
          ${this.dikeBoundingBox.xMax}, ${this.dikeBoundingBox.yMin}
          ${this.dikeBoundingBox.xMin}, ${this.dikeBoundingBox.yMin}
        `;
      },
      selectedLine() {
        const index = this.selection.segment;
        if (index === undefined) return '';
        const startPoint = this.validatedPoints[index];
        if (startPoint === undefined) return '';
        const endPoint = this.validatedPoints[index + 1];
        if (endPoint === undefined) return '';
        return `${startPoint.x},${startPoint.y} ${endPoint.x},${endPoint.y}`;
      },
      viewBox() {
        const padding = 5;
        const { xMin, xMax, yMin, yMax } = this.dikeBoundingBox;

        return {
          xMin: xMin - padding,
          yMin: yMin - padding,
          width: xMax - xMin + 2* padding,
          height: yMax - yMin + 2* padding,
        };
      },
      dikeBoundingBox() {
        return {
          xMin: Math.min(...this.validatedPoints.map(({x}) => x)),
          xMax: Math.max(...this.validatedPoints.map(({x}) => x)),
          yMin: Math.min(...this.validatedPoints.map(({y}) => y)),
          yMax: Math.max(
            ...this.validatedPoints.map(({y}) => y),
            this.result.waveRunUp || 0,
            this.result.dikeHeight || 0,
          ),
        };
      },
    },
  };
</script>

<style>
  .dike-profile {
    width: 100%;
    max-width: 40rem;
    height: 15rem;
    transform: scaleY(-1);
  }

  .dike-profile__line {
    fill: none;
    stroke: var(--neutral-8);
  }
  .dike-profile__line--selected {
    stroke: var(--accent-2);
  }

  .dike-profile__background {
    fill: var(--accent-3);
  }

  .dike-profile__point {
    stroke: var(--neutral-8);
  }

  .dike-profile__water {
    fill: var(--accent-4);
  }

  .dike-profile__result-dike-height {
    stroke: var(--primary-1);
  }

  .dike-profile__result-discharge {
    stroke: var(--primary-4);
  }
</style>
