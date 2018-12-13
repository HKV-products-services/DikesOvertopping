<template>
  <section class="dike-scenario-output">
    <h2 class="dike-scenario-output__title">Resultaat</h2>
    <div
      :class="{
        'dike-scenario-output__body--success':
          resultStatus === 'success',
        'dike-scenario-output__body--error':
          resultStatus === 'error',
      }"
      class="dike-scenario-output__body"
    >
      <template v-if="resultStatus === 'loading'">
        <p>Wordt berekend...</p>
      </template>
      <template>
        <p v-if="resultStatus === 'unsynced'">
          Er zijn nog geen resultaten.
        </p>
        <p
          v-if="resultStatus === 'error'"
          class="dike-scenario-output__error-message"
        >
          {{ result.message }}
        </p>
        <table v-if="resultStatus === 'success'">
          <tbody>
            <tr>
              <th
                class="
                  dike-scenario-output__detail
                  dike-scenario-output__detail--dike-height
                "
              >
                {{
                  calculationMethod === 'calculateDischarge'
                    ? 'Kruinniveau'
                    : 'Benodigd kruinniveau'
                }}
              </th>
              <td class="dike-scenario-output__value">
                {{ formatNumber(result.dikeHeight) }} m+NAP
              </td>
            </tr>
            <tr>
              <th
                class="
                  dike-scenario-output__detail
                  dike-scenario-output__detail--discharge
                "
              >
                2% Golfoploopniveau
              </th>
              <td class="dike-scenario-output__value">
                {{ formatNumber(result.waveRunUp) }} m+NAP
              </td>
            </tr>
            <tr v-if="typeof result.dikeHeight === 'number'">
              <th class="dike-scenario-output__detail">Overslagdebiet</th>
              <td class="dike-scenario-output__value">
                {{ formatNumber(cubicMeterToLiter(result.qo)) }} l/m/s
              </td>
            </tr>
          </tbody>
        </table>
      </template>
    </div>
  </section>
</template>

<script>
  import { mapState, mapGetters } from 'vuex';

  export default {
    computed: {
      ...mapState([
        'calculationMethod',
        'geometry',
        'result',
      ]),
      ...mapGetters([
        'resultStatus',
      ]),
    },
    methods: {
      cubicMeterToLiter(value) {
        return value * 1000;
      },
      formatNumber(value) {
        return value.toLocaleString(
          'nl-NL',
          { minimumFractionDigits: 2, maximumFractionDigits: 2, }
        );
      },
    }
  };
</script>

<style>
  .dike-scenario-output__body {
    border-left-style: solid;
    border-left-width: 4px;
    padding-left: 0.6rem;
  }

  .dike-scenario-output__body--success {
    border-left-color: var(--accent-6);
  }

  .dike-scenario-output__body--error {
    border-left-color: var(--accent-5);
  }

  .dike-scenario-output__title {
    margin-bottom: 1rem;
  }

  .dike-scenario-output__error-message {
    margin-bottom: 1rem;
  }

  .dike-scenario-output__detail {
    font-weight: bold;
    margin-right: 1rem;
  }

  .dike-scenario-output__detail,
  .dike-scenario-output__value {
    margin-bottom: 1rem;
    display: inline-block;
  }

  .dike-scenario-output__detail--discharge {
    border-bottom: 2px dotted var(--primary-4);
  }

  .dike-scenario-output__detail--dike-height {
    border-bottom: 2px solid var(--primary-1);
  }
</style>
