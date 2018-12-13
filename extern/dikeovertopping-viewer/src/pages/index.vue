<template>
  <main class="page">
    <div
      v-if="!calculationMethod"
      class="page__layer page__layer--centered"
    >
      <app-intro/>
    </div>
    <calculation-form />
    <transition name="transition--fade">
      <div v-if="calculationMethod">
        <div class="page__layer page__layer--centered page__result">
          <div class="page__result-profile">
            <dikeProfile />
          </div>
          <div class="page__result-output">
            <dikeScenarioOutput />
          </div>
        </div>
        <div class="page__background--alternate">
          <div class="page__layer page__layer--centered">
            <div class="page__input">
              <dikeScenarioInput />
              <dikeForm />
            </div>
          </div>
        </div>
      </div>
    </transition>
  </main>
</template>

<script>
  import { mapState } from 'vuex';
  import appIntro from '../components/app-intro.vue';
  import calculationForm from '../components/calculation-form.vue';
  import dikeForm from '../components/dike-form.vue';
  import dikeProfile from '../components/dike-profile.vue';
  import dikeScenarioInput from '../components/dike-scenario-input.vue';
  import dikeScenarioOutput from '../components/dike-scenario-output.vue';

  export default {
    components: {
      appIntro,
      calculationForm,
      dikeForm,
      dikeProfile,
      dikeScenarioInput,
      dikeScenarioOutput,
    },
    computed: {
      ...mapState(['calculationMethod']),
    },
  };
</script>

<style>
  .page__layer {
    margin-bottom: 2rem;
    padding-right: 1rem;
    padding-left: 1rem;
  }

  .page__layer--centered {
    max-width: var(--primary-layout-width);
    margin-right: auto;
    margin-left: auto;
  }

  .page__background--alternate {
    padding-top: 2rem;
    padding-bottom: 1rem;
    background-color: var(--primary-3);
  }

  .page__result {
    display: flex;
    flex-direction: column;
    align-items: center;
  }

  .page__result-profile {
    width: 60%;
  }

  .page__result-output {
    width: 40%;
  }

  .page__input {
    display: flex;
    flex-direction: column;
  }

  @media(min-width: 40rem) {
    .page__result {
      flex-direction: row;
    }
  }

  @media(min-width: 52rem) {
    .page__input {
      flex-direction: row;
    }
  }
</style>
