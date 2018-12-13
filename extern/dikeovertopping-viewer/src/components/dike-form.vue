<template>
  <form class="dike-form" @keyup.enter="addPoint">
    <h2>Dijkprofiel</h2>
    <table class="dike-form__table">
      <thead>
        <tr class="dike-form__head">
          <th class="dike-form__head__section" colspan="2">Van</th>
          <th class="dike-form__head__section" colspan="2">Tot</th>
          <th></th>
        </tr>
        <tr>
          <th class="dike-form__head-field">
            Afstand <span class="dike-form__head-notation">(m)</span>
          </th>
          <th class="dike-form__head-field">
            Hoogte <span class="dike-form__head-notation">(m+NAP)</span>
          </th>
          <th class="dike-form__head-field">
            Afstand <span class="dike-form__head-notation">(m)</span>
          </th>
          <th class="dike-form__head-field">
            Hoogte <span class="dike-form__head-notation">(m+NAP)</span>
          </th>
          <th class="dike-form__head-field">
            Ruwheid
          </th>
        </tr>
      </thead>
      <tbody>
        <tr
          v-for="(row, index) in points"
          :key="index"
          :class="{ 'dike-form__row--active': index === selection.segment }"
          @focusin="selectSegment(index)"
          @focusout="deselect()"
        >
          <template v-if="points[index + 1]">
            <td class="dike-form__field">
              <input
                v-model.number="row.x"
                step="any"
                required
                type="number"
                @focus="selectPoint(points[index])"
              >
            </td>
            <td class="dike-form__field">
              <input
                v-model.number="row.y"
                step="any"
                required
                type="number"
                @focus="selectPoint(points[index])"
              >
            </td>
            <td class="dike-form__field dike-form__field--alt">
              <input
                v-model.number="points[index + 1].x"
                step="any"
                required
                type="number"
                @focus="selectPoint(points[index + 1])"
              >
            </td>
            <td class="dike-form__field dike-form__field--alt">
              <input
                v-model.number="points[index + 1].y"
                step="any"
                required
                type="number"
                @focus="selectPoint(points[index + 1])"
              >
            </td>
            <td class="dike-form__field">
              <input
                v-model.number="row.roughness"
                min="0.5"
                max="1.0"
                step="any"
                required
                type="number"
                title="Factor moet tussen 0,5 en 1,0 zijn."
              >
            </td>
            <td class="dike-form__field">
              <button
                :disabled="!(points.length > 2)"
                class="button button--danger"
                type="button"
                title="Delete segment"
                @click="deleteSegment(index)"
              >
                ⨯
              </button>
            </td>
          </template>
        </tr>
      </tbody>
    </table>
    <button
      class="button button--default dike-form__button"
      type="button"
      @click="$store.commit('addPoint')"
    >
      Segment toevoegen
    </button>
  </form>
</template>

<script>
  import { mapMutations, mapState } from 'vuex';
  import { mapMultiRowFields } from 'vuex-map-fields';

  export default {
    computed: {
      ...mapMultiRowFields([
        'points'
      ]),
      ...mapState([
        'selection',
      ]),
    },
    methods: {
      ...mapMutations([
        'deselectPoint',
        'selectPoint',
        'deselectSegment',
        'selectSegment',
      ]),
      addPoint(event) {
        // ctrlKey modifier is reserved
        if (!event.ctrlKey) {
          this.$store.commit('addPoint');
        }
      },
      deselect() {
        this.deselectPoint();
        this.deselectSegment();
      },
      deleteSegment(index) {
        this.deselect();
        this.$store.commit('deletePoint', index);
      },
    }
  };
</script>

<style>
  .dike-form {
    margin-bottom: 1rem;
  }

  .dike-form__table {
    margin-top: 1rem;
    margin-bottom: 1rem;
    border-spacing: 0;
  }

  .dike-form__head-field {
    padding-top: 0.4rem;
    vertical-align: top;
  }

  .dike-form__head-field,
  .dike-form__field {
    padding-top: 0.4rem;
    padding-right: 0.5rem;
    padding-bottom: 0.4rem;
    padding-left: 0.5rem;
  }

  .dike-form__head-notation {
    font-weight: normal;
    display: block;
  }

  .dike-form__field-notation {
    font-weight: normal;
  }

  .dike-form__head {
    color: var(--primary-1);
    margin-bottom: 1rem;
  }

  .dike-form__head__section::after {
    content: "";
    display: block;
    margin: 0 auto;
    width: 80%;
    padding-top: 0.2rem;
    border-bottom: 2px solid var(--primary-5);
  }

  .dike-form__field--alt {
    color: var(--neutral-5);
  }

  .dike-form__row--active {
    background-color: var(--primary-5);
  }

  .dike-form__button {
    margin-left: .5rem;
  }
</style>
