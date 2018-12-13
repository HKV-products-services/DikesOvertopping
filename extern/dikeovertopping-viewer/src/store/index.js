import axios from 'axios';
import deepEqual from 'fast-deep-equal';
import pick from '1-liners/pick';
import Vuex from 'vuex';
import { getField, updateField } from 'vuex-map-fields';

const createPoint = ({ x = 0, y = 0, roughness = 1 }) => ({
  x,
  y,
  roughness,
});

const getCoordinates = points => ({
  xCoords: points.map(point => point.x),
  yCoords: points.map(point => point.y),
  roughness: points.map(point => point.roughness),
});

const overtoppingApi = axios.create({
  method: 'post',
  baseURL: process.env.dikeOvertoppingApi,
  headers: {
    'Content-Type': 'application/json',
  },
});

const getInputState = state => pick(
  [
    'calculationMethod',
    'discharge',
    'points',
    'load',
    'geometry',
  ],
  // Clone state so there are no value references
  JSON.parse(JSON.stringify(state))
);

export default () =>
  new Vuex.Store({
    state: () => ({
      points: [createPoint({ x: -20, y: -5 }), createPoint({ x: 20, y: 5 })],
      load: {
        waterLevel: 0,
        height: 1,
        period: 4,
        direction: 0,
      },
      geometry: {
        normal: 0,
        dikeHeight: 5,
      },
      selection: {
        point: undefined,
        segment: undefined,
      },
      discharge: 1,
      result: {},
      calculationMethod: undefined,
      isCalculating: false,
      inputSnapshot: {},
    }),
    getters: {
      getField,
      resultStatus: state => {
        if (!deepEqual(state.inputSnapshot, getInputState(state))) {
          return 'unsynced';
        } else if (state.isCalculating) {
          return 'loading';
        } else if (!state.result.success) {
          return 'error';
        } else if (state.result.success) {
          return 'success';
        }
      },
    },
    actions: {
      overtoppingApi({ commit, state }, endpoint) {
        commit('startCalculation');
        const scenarioData = {
          load: state.load,
          geometry: {
            ...state.geometry,
            ...getCoordinates(state.points),
          },
          discharge: state.discharge / 1000,
        };

        overtoppingApi({
          url: 'validate',
          data: scenarioData,
        })
          .then(({ data }) =>
            data.success
              ? overtoppingApi({
                  url: endpoint,
                  data: scenarioData,
                })
                  .then(({ data }) => data)
              : data
          )
          .then(data => commit('setResult', {
            ...data,
            ...(state.calculationMethod === 'calculateDischarge'
              && { dikeHeight: state.geometry.dikeHeight }),
            waterLevel: state.load.waterLevel,
            waveRunUp: data.z2 + state.load.waterLevel,
          }))
          .catch(error => commit('setResult', error));
      },
    },
    mutations: {
      updateField,
      addPoint({ points }) {
        const lastPoint = [...points].pop();

        points.push(createPoint({
          x: lastPoint.x + 1,
          y: lastPoint.y + 1,
        }));
      },
      deletePoint({ points }, index) {
        points.splice(index + 1, 1);
      },
      deselectPoint(state) {
        state.selection.point = undefined;
      },
      selectPoint(state, point) {
        state.selection.point = point;
      },
      deselectSegment(state) {
        state.selection.segment = undefined;
      },
      selectSegment(state, index) {
        state.selection.segment = index;
      },
      setResult(state, newResult) {
        state.result = newResult;
        state.inputSnapshot = getInputState(state);
        state.isCalculating = false;
      },
      startCalculation(state) {
        state.isCalculating = true;
      },
    },
  });
