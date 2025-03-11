"""
Pytest configuration file.
"""
import pytest
import sys
import subprocess
from pathlib import Path
from pytest_tests.test_overtopping import save_benchmark_results

def pytest_sessionfinish(session, exitstatus):
    """Called after whole test run finished."""
    # Save benchmark results
    save_benchmark_results()
    
    # Run visualization script if tests were successful
    if exitstatus == 0:
        try:
            script_path = Path(__file__).parent / 'visualize_results.py'
            subprocess.run([sys.executable, str(script_path)], check=True)
            print("Visualization generated successfully.")
        except Exception as e:
            print(f"Error generating visualization: {e}")

@pytest.fixture
def benchmark(benchmark):
    """Customize the benchmark fixture."""
    # Configure the benchmark fixture with pedantic mode
    # This ensures more accurate and consistent results
    return benchmark.pedantic(
        warmup=True,
        warmup_iterations=100000,
        rounds=100000,
        iterations=10000,
        max_time=10.0
    ) 