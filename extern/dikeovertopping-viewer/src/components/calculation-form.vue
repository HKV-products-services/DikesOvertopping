<template>
  <form
    class="page__layer calculation-form"
    @submit.prevent="calculate()"
  >
    <div class="page__layer--centered">
      <label>
        <span class="calculation-form__label">Wat wilt u berekenen?</span>
        <select
          v-model="calculationMethod"
          class="calculation-form__select"
        >
          <option :value="undefined">(kies een model)</option>
          <option
            v-for="option in methods"
            :key="option.value"
            :value="option.value">{{ option.label }}</option>
        </select>
      </label>
      <button
        v-if="calculationMethod"
        type="submit"
        class="button button--primary"
      >
        Bereken
      </button>
    </div>
  </form>
</template>

<script>
  import { mapFields } from 'vuex-map-fields';

  export default {
    data() {
      return {
        methods: [
          { value: 'calculateDischarge', label: 'debiet bij kruinniveau' },
          { value: 'calculateHeight', label: 'kruinniveau bij debiet' },
        ]
      };
    },
    computed: {
      ...mapFields(['calculationMethod']),
    },
    mounted () {
      // Bind global ctrl-enter to calculate button
      window.onkeyup = event => {
        const key = event.key;
        if (key === "Enter" && event.ctrlKey) {
          event.preventDefault();
          this.calculate();
        }
      };
    },
    methods: {
      calculate() {
        if (!this.calculationMethod) return;
        this.$store.dispatch('overtoppingApi', this.calculationMethod);
      },
    },
  };
</script>

<style>
  .calculation-form {
    background-color: var(--neutral-1);
    padding-top: 1em;
    padding-bottom: 1em;
  }

  .calculation-form__label {
    display: inline-block;
    font-weight: bold;
  }

  .calculation-form__select {
    font-size: 1.1rem;
    height: 2.75rem;
    display: inline-block;
    margin-left: .5rem;
    margin-right: .5rem;
  }
</style>
