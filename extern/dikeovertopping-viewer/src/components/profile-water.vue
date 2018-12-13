<template>
  <g class="profile-water">
    <polygon :points="points" />
    <!-- Position on left side of water -->
    <g :transform="`translate(${bounds.xMin}, ${waterLevel - (waveHeight / 2)})`">
      <!-- Scale based on wave height -->
      <g :transform="`scale(1, ${waveHeight / 2})`">
        <path fill="#fff" opacity=".5" d="M4.5,0C4,0,3.5,0.5,3,1H0v1h6V1C5.5,0.5,5,0,4.5,0z" />
        <path d="M3,1c0.5-0.5,1-1,1.5-1H0v1c0.5,0.5,1,1,1.5,1S2.5,1.5,3,1z"/>
      </g>
    </g>
  </g>
</template>

<script>
export default {
  props: {
    bounds: {
      type: Object,
      default() { return {
          xMin: 0,
          xMax: 0,
          yMin: 0,
          yMax: 0
        };}
    },
    waterLevel: {
      type: Number,
      default: 0,
    },
    waveHeight: {
      type: Number,
      default: 0,
    }
  },
  computed: {
    points () {
      return `
        ${this.bounds.xMin}, ${this.waterLevel || 0}
        ${this.bounds.xMax}, ${this.waterLevel || 0}
        ${this.bounds.xMax}, ${this.bounds.yMin}
        ${this.bounds.xMin}, ${this.bounds.yMin}
      `;
    }
  },

};
</script>

<style>
  .profile-water {
    fill: var(--accent-4);
  }
</style>
