<template>
  <section class="dike-scenario-input">
    <section class="dike-scenario-input__group">
      <h2 class="dike-scenario-input__title">Dijk</h2>
      <label
        v-if="calculationMethod === 'calculateDischarge'"
        class="dike-scenario-input__label">
        <span class="dike-scenario-input__text">
          Kruinniveau
          <span class="dike-scenario-input__unit">(m+NAP)</span>
        </span>
        <input
          v-model.number="dikeHeight"
          min="0"
          step="any"
          required
          class="dike-scenario-input__field"
          type="number"
          title="Kruinniveau moet groter of gelijk aan 0 zijn."
        >
      </label>
      <label
        v-if="calculationMethod === 'calculateHeight'"
        class="dike-scenario-input__label">
        <span class="dike-scenario-input__text">
          Max. debiet
          <span class="dike-scenario-input__unit">(l/m/s)</span>
        </span>
        <input
          v-model.number="discharge"
          min="0"
          step="any"
          required
          class="dike-scenario-input__field"
          type="number"
          title="Debiet moet groter of gelijk aan 0 zijn."
        >
      </label>
      <label class="dike-scenario-input__label">
        <span class="dike-scenario-input__text">
          Dijkoriëntatie
          <span class="dike-scenario-input__unit">(° N)</span>
        </span>
        <input
          v-model="normal"
          min="0"
          max="360"
          step="any"
          required
          class="dike-scenario-input__field"
          type="number"
          title="Dijkoriëntatie moet tussen 0 en 360 zijn."
        >
      </label>
    </section>
    <section class="dike-scenario-input__group">
      <h2 class="dike-scenario-input__title">Water</h2>
      <label class="dike-scenario-input__label">
        <span class="dike-scenario-input__text">
          Waterstand
          <span class="dike-scenario-input__unit">(m+NAP)</span>
        </span>
        <input
          v-model.number="waterLevel"
          step="any"
          class="dike-scenario-input__field"
          required
          type="number"
        >
      </label>
      <label class="dike-scenario-input__label">
        <span class="dike-scenario-input__text">
          Golfhoogte
          <span class="dike-scenario-input__unit">(m)</span>
        </span>
        <input
          v-model.number="height"
          min="0"
          step="any"
          required
          class="dike-scenario-input__field"
          type="number"
          title="Golfhoogte moet groter of gelijk aan 0 zijn."
        >
      </label>
      <label class="dike-scenario-input__label">
        <span class="dike-scenario-input__text">
          Golfperiode
          <span class="dike-scenario-input__unit">(s)</span>
        </span>
        <input
          v-model="period"
          min="0"
          step="any"
          required
          class="dike-scenario-input__field"
          type="number"
          title="Golfperiode moet groter of gelijk aan 0 zijn."
        >
      </label>
      <label class="dike-scenario-input__label">
        <span class="dike-scenario-input__text">
          Golfrichting
          <span class="dike-scenario-input__unit">(° N)</span>
        </span>
        <input
          v-model.number="direction"
          min="0"
          max="360"
          step="any"
          required
          class="dike-scenario-input__field"
          type="number"
          title="Golfrichting moet tussen 0 en 360 zijn."
        >
      </label>
    </section>
  </section>
</template>

<script>
  import { mapFields } from 'vuex-map-fields';

  export default {
    computed: {
      ...mapFields([
        'calculationMethod',
        'load.waterLevel',
        'load.height',
        'load.period',
        'load.direction',
        'geometry.normal',
        'geometry.dikeHeight',
        'discharge',
      ]),
    },
    methods: {
      setScenarioFromForm (keyName, event) {
        this.$store.commit('setScenario', { [keyName]: event.target.value });
      }
    },
  };
</script>

<style>
  .dike-scenario-input {
    display: flex;
    flex-direction: column;
    max-width: 40em;
    width: 100%;
  }

  .dike-scenario-input__title {
    margin-bottom: 1rem;
  }

  .dike-scenario-input__label {
    display: inline-block;
    width: 45%;
    margin-right: 1rem;
  }

  .dike-scenario-input__text {
    display: inline-block;
    width: 10rem;
    margin-bottom: 0.4rem;
    font-weight: bold;
  }

  .dike-scenario-input__unit {
    font-weight: normal;
  }

  .dike-scenario-input__field {
    margin-bottom: 1rem;
  }
</style>
