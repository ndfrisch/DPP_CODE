export void scan_ispc(uniform float output[], uniform float input[], uniform int n) {
    for (uniform int d = 0; d <= log2(n-1); d++) {
        foreach (k = 0 ... n) {
            if (i >= 2^j) {
                output[i] = input[i] + input[i - 2^j];
            } else {
                output[i] = input[i];
            }
        }
    }
}
