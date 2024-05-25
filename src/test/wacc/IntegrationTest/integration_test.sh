# #!/bin/bash

# # Define the source file name
# SOURCE_FILE="assembly_sample/print.s"

# # Define the output executable name
# OUTPUT_EXE="assembly_sample/print"

# # Compile the assembly code
# arm-linux-gnueabi-gcc -o "$OUTPUT_EXE" -z noexecstack -march=armv6 "$SOURCE_FILE"

# # Check if compilation was successful
# if [ $? -eq 0 ]; then
#     echo "Compilation successful."

#     # Execute the program using qemu-arm
#     qemu-arm -L /usr/arm-linux-gnueabi/ "$OUTPUT_EXE"
# else
#     echo "Compilation failed. Exiting."
# fi

#!/bin/bash

# Define color codes
RED='\033[0;31m' # Red color
GREEN='\033[0;32m' # Green color
NC='\033[0m'     # No color

# Define the list of files to test along with their expected exit codes
declare -A files_to_test=(
    ["test_cases/valid/advanced/ticTacToe.wacc"]="0"
    ["test_cases/valid/array/array.wacc"]="0"
    ["test_cases/valid/runtimeErr/divideByZero/divideByZero.wacc"]="0"
    ["test_cases/invalid/syntaxErr/function/functionLateDefine.wacc"]="100"
    ["test_cases/invalid/syntaxErr/sequence/doubleSeq.wacc"]="100"
    ["test_cases/invalid/syntaxErr/if/ifiErr.wacc"]="100"
    ["test_cases/invalid/semanticErr/multiple/messyExpr.wacc"]="200"
    ["test_cases/invalid/semanticErr/array/arrayIndexNotInt.wacc"]="200"
    ["test_cases/invalid/semanticErr/read/readIntoBadSnd.wacc"]="200"
)

test_num=0
failed=0

declare -A failed_test_names
declare -A failed_test_codes
scala-cli --power package . --server=false --jvm system --force -o wacc-compiler
for file_name in "${!files_to_test[@]}"; do
    expected_exit_code="${files_to_test[$file_name]}"
    ./compile "$file_name"
    # Check the exit status of the code
    actual_exit_code=$?
    ((test_num++))

    if [ "$actual_exit_code" != "$expected_exit_code" ]; then
        echo -e "${RED}Failed: $file_name${NC}"
        echo -e "${RED}expected exit: $expected_exit_code, actual exit: $actual_exit_code${NC}"
        ((failed++))
        failed_test_names[$failed]=$file_name
        failed_test_codes[$failed]=$actual_exit_code
        # exit 1
    else
        echo -e "${GREEN}Passed: $file_name${NC}"
    fi
done

for (( i=1; i<=failed; i++ )); do
    echo -e "${RED}Failed: ${failed_test_names[$i]} - expected exit: ${files_to_test[${failed_test_names[$i]}]}, actual exit: ${failed_test_codes[$i]}${NC}"
done

if [[ $failed == 0 ]]; then
    echo -e "${GREEN}All $test_num tests passed${NC}"
else 
    echo -e "${RED}$failed/$test_num tests failed${NC}"
fi

if [[ $test_num != 0 && $failed != 0 ]]; then
    exit 1
fi
