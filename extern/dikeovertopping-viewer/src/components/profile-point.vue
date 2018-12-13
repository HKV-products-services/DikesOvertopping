<template>
  <line
    :x1="point.x"
    :x2="point.x"
    :y1="point.y"
    :y2="point.y"
    :stroke-width="scale * 10"
    :class="{ 'profile-point--selected': selected }"
    stroke-linecap="round"
    class="profile-point"
  />
</template>

<script>
export default {
  props: {
    point: {
      type: Object,
      default () { return { x:0, y:0 }; },
      validator: point => (
        point.hasOwnProperty('x')
        && point.hasOwnProperty('y')
      )
    },
    scale: {
      type: Number,
      default: 1
    },
    selected: {
      type: Boolean,
      default: false
    },

  }
};
</script>

<style>
  .profile-point {
    stroke: var(--neutral-8);
  }
  .profile-point--selected {
    stroke: var(--accent-2);
  }
</style>
