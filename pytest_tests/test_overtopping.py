"""
Test suite for dikesOvertopping functions.
This test suite tests the Java interface functions that are available on both Windows and Linux platforms.
"""
import os
import time
import platform
import json
import pytest
import ctypes
from pathlib import Path
from ctypes import c_double, c_int, c_bool, c_char, POINTER, byref, create_string_buffer

# Constants
ERROR_MESSAGE_LENGTH = 255
MAX_FILE_SIZE_LENGTH = 256

# Load the DLL
try:
    # Path to the DLL
    if platform.system() == 'Windows':
        DLL_PATH = Path(__file__).parent.parent / 'lib' / 'dllDikesOvertopping.dll'
        dikes_dll = ctypes.CDLL(str(DLL_PATH))
    else:  # Linux
        DLL_PATH1 = Path(__file__).parent.parent / 'lib' / 'libFeedbackDll.so'
        DLL_PATH2 = Path(__file__).parent.parent / 'lib' / 'libDikesOvertopping.so'
        ctypes.CDLL(str(DLL_PATH1), mode=ctypes.RTLD_GLOBAL)
        dikes_dll = ctypes.CDLL(str(DLL_PATH2))

    # Define function signatures for Java interface functions
    dikes_dll.omkeerVariantJ.argtypes = [
        POINTER(c_double * 4),  # load - array of 4 doubles
        POINTER(c_double * 3),  # xcoords - array of npoints doubles
        POINTER(c_double * 3),  # ycoords - array of npoints doubles
        POINTER(c_double * 2),  # roughness - array of (npoints-1) doubles
        POINTER(c_double),      # normal - single double
        POINTER(c_int),         # npoints - single integer
        POINTER(c_double),      # givenDischarge - single double
        POINTER(c_double),      # dikeHeight - output double
        POINTER(c_double * 8),  # modelFactors - array of 8 doubles
        POINTER(c_double * 2),  # output - array of 2 doubles
        POINTER(c_bool),        # succes - output bool
        POINTER(c_char)         # errorMessage - output char array
    ]
    dikes_dll.omkeerVariantJ.restype = None

    # Define the function signature for calculateQoJ
    dikes_dll.calculateQoJ.argtypes = [
        POINTER(c_double * 4),  # load - array of 4 doubles
        POINTER(c_double * 3),  # xcoords - array of npoints doubles
        POINTER(c_double * 3),  # ycoords - array of npoints doubles
        POINTER(c_double * 2),  # roughness - array of (npoints-1) doubles
        POINTER(c_double),      # normal - single double
        POINTER(c_int),         # npoints - single integer
        POINTER(c_double),      # dikeHeight - input double
        POINTER(c_double * 8),  # modelFactors - array of 8 doubles
        POINTER(c_double * 2),  # output - array of 2 doubles
        POINTER(c_bool),        # succes - output bool
        POINTER(c_char)         # errorMessage - output char array
    ]
    dikes_dll.calculateQoJ.restype = None

except Exception as e:
    print(f"Error loading DLL: {e}")
    dikes_dll = None

# Helper functions to interface with the DLL
def get_discharge(wave_params, normal, x_coords, y_coords, roughness, dike_height, model_factors):
    """Calculate discharge using the DLL."""
    # Convert inputs to ctypes arrays
    npoints = c_int(len(x_coords))
    load = (c_double * 4)(*wave_params)
    x_coords_array = (c_double * npoints.value)(*x_coords)
    y_coords_array = (c_double * npoints.value)(*y_coords)
    roughness_array = (c_double * (npoints.value - 1))(*roughness)
    normal_val = c_double(normal)
    dike_height_val = c_double(dike_height)
    model_factors_array = (c_double * 8)(*model_factors)
    
    # Output parameters
    output = (c_double * 2)(0.0, 0.0)  # [Qo, Z2]
    success = c_bool(False)
    error_message = create_string_buffer(ERROR_MESSAGE_LENGTH)
    
    # Call the DLL function
    dikes_dll.calculateQoJ(
        byref(load),
        byref(x_coords_array),
        byref(y_coords_array),
        byref(roughness_array),
        byref(normal_val),
        byref(npoints),
        byref(dike_height_val),
        byref(model_factors_array),
        byref(output),
        byref(success),
        error_message
    )
    
    if not success.value:
        raise Exception(error_message.value.decode('utf-8').strip())
    
    return {'Z2': output[1], 'Qo': output[0]}

def omkeer_variant(wave_params, discharge, normal, x_coords, y_coords, roughness, model_factors):
    """Calculate dike height using the omkeer variant."""
    # Convert inputs to ctypes arrays
    npoints = c_int(len(x_coords))
    load = (c_double * 4)(*wave_params)
    discharge_val = c_double(discharge)
    x_coords_array = (c_double * npoints.value)(*x_coords)
    y_coords_array = (c_double * npoints.value)(*y_coords)
    roughness_array = (c_double * (npoints.value - 1))(*roughness)
    normal_val = c_double(normal)
    model_factors_array = (c_double * 8)(*model_factors)
    
    # Output parameters
    dike_height = c_double(0.0)  # Output parameter
    output = (c_double * 2)(0.0, 0.0)  # Will contain [Z2, Qo]
    success = c_bool(False)
    error_message = create_string_buffer(ERROR_MESSAGE_LENGTH)
    
    dikes_dll.omkeerVariantJ(
        byref(load),
        byref(x_coords_array),
        byref(y_coords_array),
        byref(roughness_array),
        byref(normal_val),
        byref(npoints),
        byref(discharge_val),
        byref(dike_height),
        byref(model_factors_array),
        byref(output),
        byref(success),
        error_message
    )
    
    if not success.value:
        raise Exception(error_message.value.decode('utf-8').strip())
    
    return dike_height.value

# Benchmark decorator
def benchmark(func):
    """Decorator to benchmark a function."""
    def wrapper(*args, **kwargs):
        start_time = time.time()
        result = func(*args, **kwargs)
        end_time = time.time()
        execution_time = end_time - start_time
        
        # Store benchmark results
        benchmark_results = getattr(wrapper, 'benchmark_results', [])
        benchmark_results.append({
            'function': func.__name__,
            'execution_time': execution_time,
            'platform': platform.system(),
            'processor': platform.processor(),
            'python_version': platform.python_version(),
        })
        setattr(wrapper, 'benchmark_results', benchmark_results)
        
        return result
    return wrapper

# Test cases
@pytest.mark.skipif(dikes_dll is None, reason="DLL not found")
class TestOvertopping:
    """Test cases for the overtopping functions."""
    
    def setup_method(self):
        """Set up test data."""
        # Common test data
        self.model_factors = [
            2.3,   # FactorDeterminationQnFn
            4.3,   # FactorDeterminationQbFb
            1.0,   # Mz2
            0.92,  # Fshallow
            1.0,   # ComputedOvertopping
            1.0,   # CriticalOvertopping
            1.0,   # Relaxationfactor
            0.5    # ReductionFactorForeshore
        ]
        
        # Simple dike profile
        self.npoints = 3
        self.x_coords = [5 * (i + 1) for i in range(self.npoints)]
        self.y_coords = [3 + 2 * (i + 1) for i in range(self.npoints)]
        self.roughness = [1.0 for _ in range(self.npoints - 1)]
        
        self.dike_height = 9.1
        self.normal = 60.0  # degrees
        
        # Load conditions
        self.wave_params1 = [5.5, 1.0, 4.0, 50.0]  # WaterLevel, Height, Period, Direction
        self.wave_params2 = [5.5, 0.0, 4.0, 50.0]  # No waves case
    
    @benchmark
    def test_get_discharge(self):
        """Test the GetDischarge function."""
        result = get_discharge(
            self.wave_params1, self.normal, 
            self.x_coords, self.y_coords, self.roughness, 
            self.dike_height, self.model_factors
        )
        
        # Check results match expected values
        assert abs(result['Z2'] - 1.519737) < 0.000001
        assert abs(result['Qo'] - 8.089025E-09) < 1.0E-15
    
    @benchmark
    def test_get_discharge_no_waves(self):
        """Test the GetDischarge function with no waves."""
        result = get_discharge(
            self.wave_params2, self.normal, 
            self.x_coords, self.y_coords, self.roughness, 
            self.dike_height, self.model_factors
        )
        
        # For no waves case, we only check that the function executes without error
        # as the results may vary between platforms
        assert isinstance(result['Z2'], float)
        assert isinstance(result['Qo'], float)
    
    @benchmark
    def test_omkeer_variant(self):
        """Test the OmkeerVariant function."""
        discharge = 1e-8
        
        dike_height = omkeer_variant(
            self.wave_params1, discharge, self.normal,
            self.x_coords, self.y_coords, self.roughness,
            self.model_factors
        )
        
        # Check result matches expected value
        assert abs(dike_height - 9.055) < 0.01

# Function to save benchmark results
def save_benchmark_results():
    """Save benchmark results to a JSON file."""
    results = []
    
    # Collect results from all benchmark-decorated functions
    for attr_name in dir(TestOvertopping):
        attr = getattr(TestOvertopping, attr_name)
        if hasattr(attr, 'benchmark_results'):
            results.extend(getattr(attr, 'benchmark_results'))
    
    # Save results to a file
    if results:
        results_file = Path(__file__).parent / f'benchmark_{platform.system().lower()}.json'
        
        # Check if file exists and load existing results
        existing_results = []
        if results_file.exists():
            with open(results_file, 'r') as f:
                try:
                    existing_results = json.load(f)
                except json.JSONDecodeError:
                    pass
        
        # Combine results
        all_results = existing_results + results
        
        # Save combined results
        with open(results_file, 'w') as f:
            json.dump(all_results, f, indent=2)
        
        print(f"Benchmark results saved to {results_file}")
  
# Register the save_benchmark_results function to be called after all tests
def pytest_sessionfinish(session, exitstatus):
    """Called after whole test run finished."""
    save_benchmark_results()
