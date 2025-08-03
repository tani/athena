#!/bin/bash
# run-benchmarks.sh — Script to run performance benchmarks across implementations
# Copyright © 2025 Masaya Taniguchi
# Released under the GNU General Public License v3.0

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ATHENA_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
DEFAULT_IMPLEMENTATIONS="sbcl racket gauche"
IMPLEMENTATIONS="${BENCHMARK_IMPLEMENTATIONS:-$DEFAULT_IMPLEMENTATIONS}"
OUTPUT_DIR="${BENCHMARK_OUTPUT_DIR:-$SCRIPT_DIR/results}"
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")

# Function to print colored output
print_status() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Function to check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Function to run Common Lisp benchmarks
run_cl_benchmarks() {
    local impl=$1
    print_status "Running Common Lisp benchmarks with $impl..."
    
    cd "$ATHENA_ROOT"
    
    case $impl in
        sbcl)
            if command_exists sbcl; then
                sbcl --script "$SCRIPT_DIR/performance-suite.lisp" \
                    > "$OUTPUT_DIR/cl_${impl}_${TIMESTAMP}.log" 2>&1 \
                    || print_warning "SBCL benchmarks failed"
            else
                print_warning "SBCL not found, skipping"
            fi
            ;;
        ccl)
            if command_exists ccl; then
                ccl --batch --load "$SCRIPT_DIR/performance-suite.lisp" \
                    > "$OUTPUT_DIR/cl_${impl}_${TIMESTAMP}.log" 2>&1 \
                    || print_warning "CCL benchmarks failed"
            else
                print_warning "CCL not found, skipping"
            fi
            ;;
        *)
            print_warning "Unknown Common Lisp implementation: $impl"
            ;;
    esac
}

# Function to run Scheme benchmarks
run_scheme_benchmarks() {
    local impl=$1
    print_status "Running Scheme benchmarks with $impl..."
    
    cd "$ATHENA_ROOT"
    
    case $impl in
        racket)
            if command_exists racket; then
                racket -e "
(require \"test/benchmarks/scheme-performance.rkt\")
(run-scheme-benchmarks)
" > "$OUTPUT_DIR/scheme_${impl}_${TIMESTAMP}.log" 2>&1 \
                    || print_warning "Racket benchmarks failed"
            else
                print_warning "Racket not found, skipping"
            fi
            ;;
        gauche)
            if command_exists gosh; then
                gosh -r7 -I src -e "
(import (scheme base) (scheme write))
(load \"test/benchmarks/scheme-performance.scm\")
(run-scheme-benchmarks)
" > "$OUTPUT_DIR/scheme_${impl}_${TIMESTAMP}.log" 2>&1 \
                    || print_warning "Gauche benchmarks failed"
            else
                print_warning "Gauche not found, skipping"
            fi
            ;;
        chibi)
            if command_exists chibi-scheme; then
                chibi-scheme -I src -e "
(import (scheme base) (scheme write))
(load \"test/benchmarks/scheme-performance.scm\")
(run-scheme-benchmarks)
" > "$OUTPUT_DIR/scheme_${impl}_${TIMESTAMP}.log" 2>&1 \
                    || print_warning "Chibi benchmarks failed"
            else
                print_warning "Chibi Scheme not found, skipping"
            fi
            ;;
        *)
            print_warning "Unknown Scheme implementation: $impl"
            ;;
    esac
}

# Function to parse and compare results
analyze_results() {
    print_status "Analyzing benchmark results..."
    
    local analysis_file="$OUTPUT_DIR/analysis_${TIMESTAMP}.txt"
    
    echo "Athena Prolog Performance Analysis - $(date)" > "$analysis_file"
    echo "=================================================" >> "$analysis_file"
    echo "" >> "$analysis_file"
    
    # Find all log files from this run
    local log_files=$(find "$OUTPUT_DIR" -name "*_${TIMESTAMP}.log" 2>/dev/null)
    
    if [ -z "$log_files" ]; then
        print_warning "No benchmark results found"
        return
    fi
    
    echo "Benchmark Results Summary:" >> "$analysis_file"
    echo "" >> "$analysis_file"
    
    for log_file in $log_files; do
        local impl_name=$(basename "$log_file" | sed "s/_${TIMESTAMP}.log//")
        echo "=== $impl_name ===" >> "$analysis_file"
        
        # Extract key performance metrics
        if [ -f "$log_file" ]; then
            # Look for timing information
            grep -E "(avg|Average|ms|seconds|ops/sec)" "$log_file" 2>/dev/null | head -20 >> "$analysis_file" || true
        fi
        
        echo "" >> "$analysis_file"
    done
    
    # Generate comparison if multiple implementations
    local num_logs=$(echo "$log_files" | wc -w)
    if [ "$num_logs" -gt 1 ]; then
        echo "Cross-Implementation Comparison:" >> "$analysis_file"
        echo "================================" >> "$analysis_file"
        
        # Simple comparison logic (could be enhanced)
        for metric in "Simple Atom Unification" "Variable Binding" "Simple Fact Lookup"; do
            echo "" >> "$analysis_file"
            echo "Metric: $metric" >> "$analysis_file"
            for log_file in $log_files; do
                local impl_name=$(basename "$log_file" | sed "s/_${TIMESTAMP}.log//")
                local timing=$(grep "$metric" "$log_file" 2>/dev/null | head -1 || echo "N/A")
                echo "  $impl_name: $timing" >> "$analysis_file"
            done
        done
    fi
    
    print_success "Analysis saved to $analysis_file"
}

# Function to generate HTML report
generate_html_report() {
    local html_file="$OUTPUT_DIR/report_${TIMESTAMP}.html"
    
    print_status "Generating HTML report..."
    
    cat > "$html_file" << EOF
<!DOCTYPE html>
<html>
<head>
    <title>Athena Prolog Performance Report - $(date)</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 20px; }
        .header { background-color: #f0f0f0; padding: 20px; border-radius: 5px; }
        .implementation { margin: 20px 0; }
        .benchmark { margin: 10px 0; padding: 10px; background-color: #f9f9f9; }
        .metric { font-family: monospace; }
        .fast { color: green; }
        .slow { color: red; }
        table { border-collapse: collapse; width: 100%; }
        th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
        th { background-color: #f2f2f2; }
    </style>
</head>
<body>
    <div class="header">
        <h1>Athena Prolog Performance Report</h1>
        <p>Generated: $(date)</p>
        <p>Implementations tested: $IMPLEMENTATIONS</p>
    </div>
EOF

    # Add benchmark results for each implementation
    for log_file in $(find "$OUTPUT_DIR" -name "*_${TIMESTAMP}.log" 2>/dev/null); do
        local impl_name=$(basename "$log_file" | sed "s/_${TIMESTAMP}.log//")
        
        echo "<div class=\"implementation\">" >> "$html_file"
        echo "<h2>$impl_name</h2>" >> "$html_file"
        echo "<pre>" >> "$html_file"
        
        # Include relevant parts of the log
        head -50 "$log_file" >> "$html_file" 2>/dev/null || true
        
        echo "</pre>" >> "$html_file"
        echo "</div>" >> "$html_file"
    done
    
    cat >> "$html_file" << EOF
</body>
</html>
EOF

    print_success "HTML report saved to $html_file"
}

# Function to run continuous benchmarks
run_continuous() {
    print_status "Running continuous benchmarks (press Ctrl+C to stop)..."
    
    local interval=${BENCHMARK_INTERVAL:-300}  # 5 minutes default
    
    while true; do
        print_status "Starting benchmark cycle at $(date)"
        
        # Run benchmarks for all implementations
        for impl in $IMPLEMENTATIONS; do
            case $impl in
                sbcl|ccl)
                    run_cl_benchmarks "$impl"
                    ;;
                racket|gauche|chibi|chicken|chez|guile)
                    run_scheme_benchmarks "$impl"
                    ;;
                *)
                    print_warning "Unknown implementation: $impl"
                    ;;
            esac
        done
        
        analyze_results
        
        print_status "Benchmark cycle completed. Sleeping for $interval seconds..."
        sleep "$interval"
    done
}

# Function to show help
show_help() {
    cat << EOF
Athena Prolog Benchmark Runner

Usage: $0 [OPTIONS] [COMMAND]

Commands:
    run         Run benchmarks once (default)
    continuous  Run benchmarks continuously
    analyze     Analyze existing results
    clean       Clean old results
    help        Show this help

Options:
    -i, --implementations LIST    Comma-separated list of implementations
                                 (default: $DEFAULT_IMPLEMENTATIONS)
    -o, --output-dir DIR         Output directory for results
                                 (default: $OUTPUT_DIR)
    -h, --help                   Show this help

Environment Variables:
    BENCHMARK_IMPLEMENTATIONS   Override default implementations
    BENCHMARK_OUTPUT_DIR        Override default output directory
    BENCHMARK_INTERVAL          Interval for continuous mode (seconds)

Examples:
    $0                           # Run benchmarks with default implementations
    $0 -i "sbcl,racket"         # Run benchmarks with specific implementations
    $0 continuous               # Run continuous benchmarks
    $0 analyze                  # Analyze existing results
    
Supported implementations:
    Common Lisp: sbcl, ccl
    Scheme: racket, gauche, chibi, chicken, chez, guile
EOF
}

# Main execution
main() {
    # Parse command line arguments
    while [[ $# -gt 0 ]]; do
        case $1 in
            -i|--implementations)
                IMPLEMENTATIONS="$2"
                shift 2
                ;;
            -o|--output-dir)
                OUTPUT_DIR="$2"
                shift 2
                ;;
            -h|--help)
                show_help
                exit 0
                ;;
            run)
                COMMAND="run"
                shift
                ;;
            continuous)
                COMMAND="continuous"
                shift
                ;;
            analyze)
                COMMAND="analyze"
                shift
                ;;
            clean)
                COMMAND="clean"
                shift
                ;;
            help)
                show_help
                exit 0
                ;;
            *)
                print_error "Unknown option: $1"
                show_help
                exit 1
                ;;
        esac
    done
    
    # Set default command
    COMMAND=${COMMAND:-run}
    
    # Create output directory
    mkdir -p "$OUTPUT_DIR"
    
    print_status "Starting Athena Prolog benchmark suite"
    print_status "Output directory: $OUTPUT_DIR"
    print_status "Implementations: $IMPLEMENTATIONS"
    print_status "Timestamp: $TIMESTAMP"
    
    case $COMMAND in
        run)
            # Run benchmarks for all specified implementations
            for impl in $IMPLEMENTATIONS; do
                case $impl in
                    sbcl|ccl)
                        run_cl_benchmarks "$impl"
                        ;;
                    racket|gauche|chibi|chicken|chez|guile)
                        run_scheme_benchmarks "$impl"
                        ;;
                    *)
                        print_warning "Unknown implementation: $impl"
                        ;;
                esac
            done
            
            analyze_results
            generate_html_report
            print_success "Benchmark suite completed"
            ;;
        continuous)
            run_continuous
            ;;
        analyze)
            analyze_results
            generate_html_report
            ;;
        clean)
            print_status "Cleaning old benchmark results..."
            find "$OUTPUT_DIR" -name "*.log" -mtime +7 -delete 2>/dev/null || true
            find "$OUTPUT_DIR" -name "*.txt" -mtime +7 -delete 2>/dev/null || true
            find "$OUTPUT_DIR" -name "*.html" -mtime +7 -delete 2>/dev/null || true
            print_success "Cleanup completed"
            ;;
        *)
            print_error "Unknown command: $COMMAND"
            show_help
            exit 1
            ;;
    esac
}

# Run main function
main "$@"