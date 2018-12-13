<template>
  <footer class="footer">
    <div class="footer__lint"></div>
  </footer>
</template>

<style>
  .footer {
    display: flex;
    justify-content: center;
    position: fixed;
    bottom: 0;
    width: 100%;
    border-bottom: 4px solid var(--accent-1);
  }

  .footer__lint {
    width: 3.125rem;
    height: 1.375rem;
    background: var(--accent-1);
    display: block;
  }
</style>
