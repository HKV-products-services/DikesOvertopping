"""
Script to visualize benchmark results.
This script generates an HTML file with charts.js to compare Linux and Windows results.
"""
import json
import platform
from pathlib import Path
from datetime import datetime

def generate_html(benchmark_results):
    """Generate an HTML file with charts.js to visualize benchmark results."""
    # Group results by function and platform
    grouped_results = {}
    for result in benchmark_results:
        function_name = result['function']
        platform_name = result['platform']
        
        if function_name not in grouped_results:
            grouped_results[function_name] = {'Windows': [], 'Linux': []}
        
        if platform_name == 'Windows':
            grouped_results[function_name]['Windows'].append(result['execution_time'])
        else:
            grouped_results[function_name]['Linux'].append(result['execution_time'])
    
    # Calculate average execution times and calls per second
    avg_results = {}
    calls_per_second = {}
    for function_name, platforms in grouped_results.items():
        avg_results[function_name] = {
            'Windows': sum(platforms['Windows']) / len(platforms['Windows']) if platforms['Windows'] else 0,
            'Linux': sum(platforms['Linux']) / len(platforms['Linux']) if platforms['Linux'] else 0
        }
        calls_per_second[function_name] = {
            'Windows': 1.0 / avg_results[function_name]['Windows'] if avg_results[function_name]['Windows'] > 0 else 0,
            'Linux': 1.0 / avg_results[function_name]['Linux'] if avg_results[function_name]['Linux'] > 0 else 0
        }
    
    # Generate HTML content
    html_content = """
    <!DOCTYPE html>
    <html lang="en">
    <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>DikesOvertopping Benchmark Results</title>
        <script src="https://cdn.jsdelivr.net/npm/chart.js"></script>
        <style>
            body {
                font-family: Arial, sans-serif;
                margin: 20px;
                background-color: #f5f5f5;
            }
            .container {
                max-width: 1200px;
                margin: 0 auto;
                background-color: white;
                padding: 20px;
                border-radius: 8px;
                box-shadow: 0 0 10px rgba(0, 0, 0, 0.1);
            }
            h1, h2 {
                color: #333;
            }
            .chart-container {
                margin-bottom: 30px;
                height: 400px;
            }
            table {
                width: 100%;
                border-collapse: collapse;
                margin-top: 20px;
            }
            th, td {
                border: 1px solid #ddd;
                padding: 8px;
                text-align: left;
            }
            th {
                background-color: #f2f2f2;
            }
            tr:nth-child(even) {
                background-color: #f9f9f9;
            }
            .footer {
                margin-top: 30px;
                text-align: center;
                color: #666;
                font-size: 0.9em;
            }
        </style>
    </head>
    <body>
        <div class="container">
            <h1>DikesOvertopping Benchmark Results</h1>
            <p>Generated on: """ + datetime.now().strftime("%Y-%m-%d %H:%M:%S") + """</p>
            
            <h2>Calls Per Second Comparison</h2>
            <div class="chart-container">
                <canvas id="barChart"></canvas>
            </div>
            
            <h2>Detailed Results</h2>
            <table>
                <thead>
                    <tr>
                        <th>Function</th>
                        <th>Windows Avg Time (s)</th>
                        <th>Linux Avg Time (s)</th>
                        <th>Windows Calls/s</th>
                        <th>Linux Calls/s</th>
                        <th>Difference (%)</th>
                    </tr>
                </thead>
                <tbody>
    """
    
    # Add table rows
    for function_name, platforms in avg_results.items():
        windows_time = platforms['Windows']
        linux_time = platforms['Linux']
        windows_cps = calls_per_second[function_name]['Windows']
        linux_cps = calls_per_second[function_name]['Linux']
        
        # Calculate difference percentage
        if windows_cps > 0 and linux_cps > 0:
            diff_percent = ((linux_cps - windows_cps) / windows_cps) * 100
            diff_str = f"{diff_percent:.2f}%"
        else:
            diff_str = "N/A"
        
        html_content += f"""
                    <tr>
                        <td>{function_name}</td>
                        <td>{windows_time:.6f}</td>
                        <td>{linux_time:.6f}</td>
                        <td>{windows_cps:.2f}</td>
                        <td>{linux_cps:.2f}</td>
                        <td>{diff_str}</td>
                    </tr>
        """
    
    # Complete the HTML content
    html_content += """
                </tbody>
            </table>
            
            <div class="footer">
                <p>DikesOvertopping Benchmark Results</p>
            </div>
        </div>
        
        <script>
            // Bar chart
            const ctx = document.getElementById('barChart').getContext('2d');
            const barChart = new Chart(ctx, {
                type: 'bar',
                data: {
                    labels: """ + json.dumps(list(calls_per_second.keys())) + """,
                    datasets: [
                        {
                            label: 'Windows',
                            data: """ + json.dumps([calls_per_second[func]['Windows'] for func in calls_per_second]) + """,
                            backgroundColor: 'rgba(54, 162, 235, 0.7)',
                            borderColor: 'rgba(54, 162, 235, 1)',
                            borderWidth: 1
                        },
                        {
                            label: 'Linux',
                            data: """ + json.dumps([calls_per_second[func]['Linux'] for func in calls_per_second]) + """,
                            backgroundColor: 'rgba(255, 99, 132, 0.7)',
                            borderColor: 'rgba(255, 99, 132, 1)',
                            borderWidth: 1
                        }
                    ]
                },
                options: {
                    responsive: true,
                    maintainAspectRatio: false,
                    scales: {
                        y: {
                            beginAtZero: true,
                            title: {
                                display: true,
                                text: 'Calls Per Second'
                            }
                        },
                        x: {
                            title: {
                                display: true,
                                text: 'Function'
                            }
                        }
                    },
                    plugins: {
                        title: {
                            display: true,
                            text: 'Calls Per Second by Platform'
                        },
                        tooltip: {
                            callbacks: {
                                label: function(context) {
                                    return context.dataset.label + ': ' + context.raw.toFixed(2) + ' calls/s';
                                }
                            }
                        }
                    }
                }
            });
        </script>
    </body>
    </html>
    """
    
    return html_content

def main():
    """Main function to generate the HTML visualization."""
    # Get the paths to the benchmark results files
    linux_results_file = Path(__file__).parent / 'benchmark_linux.json'
    windows_results_file = Path(__file__).parent / 'benchmark_windows.json'
    
    # Check if the files exist
    if not linux_results_file.exists():
        print(f"Warning: Linux benchmark results file not found at {linux_results_file}")
        linux_results = []
    else:
        # Load the Linux benchmark results
        with open(linux_results_file, 'r') as f:
            linux_results = json.load(f)
    
    if not windows_results_file.exists():
        print(f"Warning: Windows benchmark results file not found at {windows_results_file}")
        windows_results = []
    else:
        # Load the Windows benchmark results
        with open(windows_results_file, 'r') as f:
            windows_results = json.load(f)
    
    # Combine the results
    benchmark_results = linux_results + windows_results
    
    if not benchmark_results:
        print("Error: No benchmark results found. Please run the tests first.")
        return
    
    # Generate the HTML content
    html_content = generate_html(benchmark_results)
    
    # Save the HTML content to a file
    output_file = Path(__file__).parent / 'benchmark_results.html'
    with open(output_file, 'w') as f:
        f.write(html_content)
    
    print(f"Visualization generated successfully.")
    print(f"Benchmark visualization saved to {output_file}")

if __name__ == "__main__":
    main() 