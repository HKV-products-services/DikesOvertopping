// Persist and rehydrate app state between page loads

import createPersistedState from 'vuex-persistedstate';

export default ({ store }) => {
  window.onNuxtReady(() => {
    createPersistedState({
      paths: [
        'calculationMethod',
        'discharge',
        'geometry',
        'load',
        'points',
      ],
    })(store);
  });
};
