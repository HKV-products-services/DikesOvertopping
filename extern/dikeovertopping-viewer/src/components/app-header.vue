<template>
  <header class="app-header">
    <img
      :src="mapUrl('logo-rijkswaterstaat.svg')"
      class="app-header__logo"
      alt="Rijksoverheid logo"
    >
    <nav class="app-header__nav">
      <ul class="app-header__nav-list">
        <nuxt-link
          class="app-header__nav-item"
          to="/"
          tag="li"
          active-class="app-header__nav-item--active"
          exact
        >
          <h1 class="app-header__title">
            Golf Overslag
            <span class="app-header__title-sub">- prototype</span>
          </h1>
        </nuxt-link>
        <nuxt-link
          class="app-header__nav-item"
          to="/about"
          tag="li"
          active-class="app-header__nav-item--active"
          exact
        >
          Achtergrond informatie
        </nuxt-link>
      </ul>
    </nav>
  </header>
</template>

<script>
  export default {
    methods: {
      mapUrl(path) {
        return process.env.ROOT_PATH + path;
      }
    }
  };
</script>
<style>
  .app-header {
    display: flex;
    flex-direction: column;
    align-items: center;
  }

  .app-header__logo {
    position: relative;
    left: 113px;
    margin-bottom: 1.4rem;
  }

  .app-header__nav {
    background: var(--accent-2);
    width: 100%;
  }

  .app-header__nav-list {
    list-style: none;
    display: flex;
    max-width: 75rem;
    margin-right: auto;
    margin-left: auto;
  }

  .app-header__nav-item {
    font-size: 1.2em;
    padding: 1rem;
    cursor: pointer;
  }

  .app-header__nav-item--active {
    background-color: var(--accent-7);
  }

  .app-header__title {
    font-size: 1em;
    max-width: var(--primary-layout-width);
    margin: 0 auto;
  }

  .app-header__title-sub {
    font-weight: normal;
  }
</style>
