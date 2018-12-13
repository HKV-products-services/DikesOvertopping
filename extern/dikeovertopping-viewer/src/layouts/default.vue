<template>
  <div>
    <appHeader/>
    <nuxt/>
    <appFooter/>
  </div>
</template>

<style>
  @import '../assets/index.css';
</style>

<script>
  import appHeader from '../components/app-header.vue';
  import appFooter from '../components/app-footer.vue';

  export default {
    components: {
      appHeader,
      appFooter,
    },
  };
</script>
