<template>
  <main class="page--about">
    <p>
      In het prototype kiezen we in grote lijnen voor<br>de 'ontwerp'-waarden van de modelinstellingen:
    </p>

    <table class="page--about__table">
      <thead>
        <tr>
          <th>Naam</th>
          <th>Waarde</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <td>computedOvertopping</td>
          <td>1.0</td>
        </tr>
        <tr>
          <td>criticalOvertopping</td>
          <td>1.0</td>
        </tr>
        <tr>
          <td>factorDeterminationQbFb</td>
          <td>4.3</td>
        </tr>
        <tr>
          <td>factorDeterminationQnFn</td>
          <td>2.3</td>
        </tr>
        <tr>
          <td>fShallow</td>
          <td>0.6778</td>
        </tr>
        <tr>
          <td>mz2</td>
          <td>1.07</td>
        </tr>
        <tr>
          <td>relaxationFactor</td>
          <td>1.0</td>
        </tr>
        <tr>
          <td>reductionFactorForeshore</td>
          <td>0.5</td>
        </tr>
      </tbody>
    </table>



  </main>
</template>

<style>
  .page--about {
    padding: 1rem;
    max-width: 75rem;
    margin-right: auto;
    margin-left: auto;
  }

  .page--about__table {
    text-align: left;
    border-spacing: 0;
    padding-top: 1rem;
  }

  .page--about__table thead {
    font-weight: bold;
  }

  .page--about__table th,
  .page--about__table td {
    padding-top: 0.6rem;
    padding-right: 2rem;
    padding-bottom: 0.4rem;
    padding-left: 0.6rem;
  }

  .page--about__table tbody > tr:nth-child(odd) {
    background-color: var(--neutral-2);
  }
</style>
