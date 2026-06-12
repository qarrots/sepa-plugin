#include <windows.h>

BOOL APIENTRY DllMain(HMODULE h, DWORD reason, LPVOID lp) {
    (void)h; (void)reason; (void)lp;
    return TRUE;
}

static float fnMA(float* d, int pos, int n) {
    int i;
    float s = 0;
    if (pos < n - 1) return 0;
    for (i = 0; i < n; i++) s += d[pos - i];
    return s / (float)n;
}

static float fnHHV(float* d, int from, int to) {
    int i;
    float hi = 0;
    if (from < 0) from = 0;
    for (i = from; i <= to; i++)
        if (d[i] > hi) hi = d[i];
    return hi;
}

static float fnLLV(float* d, int from, int to) {
    int i;
    float lo = 999999;
    if (from < 0) from = 0;
    for (i = from; i <= to; i++)
        if (d[i] < lo) lo = d[i];
    return lo;
}

static void DoSEPA(float* C, float* H, float* L, float* V,
                   int n, int mode, float* pResult) {
    int last, cnt;
    float ma50, ma150, ma200, ma200p;
    float g126, g063, g021, g250, rs;
    float a20, a40, a60, v10, v30, v50;
    float piv, dist, abs_dist;
    int ts, rss, vs, ps, total;
    float hhv20, hhv40, hhv60;
    BOOL vcp_flag, brk_flag, near_flag;

    *pResult = 0.0f;
    if (n < 250) return;

    last = n - 1;

    ma50   = fnMA(C, last, 50);
    ma150  = fnMA(C, last, 150);
    ma200  = fnMA(C, last, 200);
    ma200p = (n >= 222) ? fnMA(C, last - 22, 200) : 0;

    cnt = 0;
    if (C[last] > ma150 && C[last] > ma200) cnt++;
    if (ma150 > ma200) cnt++;
    if (ma200 > ma200p && ma200p > 0) cnt++;
    if (ma50 > ma150 && ma50 > ma200) cnt++;
    if (C[last] > ma50) cnt++;
    if (C[last] >= fnLLV(L, last - 249, last) * 1.25f) cnt++;
    if (C[last] >= fnHHV(H, last - 249, last) * 0.75f) cnt++;

    g126 = (n > 126) ? C[last] / C[last - 126] - 1 : 0;
    g063 = (n > 63)  ? C[last] / C[last - 63]  - 1 : 0;
    g021 = (n > 21)  ? C[last] / C[last - 21]  - 1 : 0;
    g250 = (n > 250) ? C[last] / C[last - 250] - 1 : 0;
    rs = 0.40f * g126 + 0.20f * g063 + 0.20f * g021 + 0.20f * g250;

    hhv20 = fnHHV(H, last - 19, last);
    hhv40 = fnHHV(H, last - 39, last);
    hhv60 = fnHHV(H, last - 59, last);
    a20 = (hhv20 - fnLLV(L, last - 19, last)) / (hhv20 + 0.001f) * 100;
    a40 = (hhv40 - fnLLV(L, last - 39, last)) / (hhv40 + 0.001f) * 100;
    a60 = (hhv60 - fnLLV(L, last - 59, last)) / (hhv60 + 0.001f) * 100;

    v10 = fnMA(V, last, 10);
    v30 = fnMA(V, last, 30);
    v50 = fnMA(V, last, 50);

    vcp_flag = (a20 < a40 && a40 < a60) && (v10 < v30 && v30 < v50);

    piv = fnHHV(H, last - 59, last - 5);
    dist = (C[last] / (piv + 0.001f) - 1) * 100;
    abs_dist = (dist >= 0) ? dist : -dist;

    brk_flag  = (C[last] > piv) && (V[last] > v50);
    near_flag = (dist >= 0 && dist <= 3);

    if (cnt == 7 && rs > 0.30f) ts = 35;
    else if (cnt == 7 && rs > 0.15f) ts = 27;
    else if (cnt >= 5) ts = 20;
    else if (cnt >= 4) ts = 10;
    else ts = 0;

    if (rs > 0.50f) rss = 25;
    else if (rs > 0.30f) rss = 20;
    else if (rs > 0.15f) rss = 15;
    else if (rs > 0) rss = 10;
    else if (rs > -0.1f) rss = 5;
    else rss = 0;

    vs = vcp_flag ? 25 : ((a20 < a40 || v10 < v30) ? 12 : 0);

    if (brk_flag && V[last] > v50 * 1.5f) ps = 15;
    else if (brk_flag) ps = 12;
    else if (near_flag) ps = 8;
    else if (dist > -5) ps = 4;
    else ps = 0;

    total = ts + rss + vs + ps;

    if (mode == 0)
        *pResult = (cnt >= 6 && C[last] > ma50 && rs > 0) ? 1.0f : 0.0f;
    else if (mode == 1)
        *pResult = (cnt == 7 && rs > 0.15f && (brk_flag || vcp_flag)) ? 1.0f : 0.0f;
    else
        *pResult = (total >= 65 && (brk_flag || near_flag) && cnt >= 5 && abs_dist <= 3) ? 1.0f : 0.0f;
}

__declspec(dllexport) int __cdecl TDX_WIDE(
    int n, float* pfOut,
    float* p1, float* p2, float* p3, float* p4)
{
    int i;
    float r = 0;
    for (i = 0; i < n; i++) pfOut[i] = 0;
    DoSEPA(p1, p2, p3, p4, n, 0, &r);
    if (n > 0) pfOut[n - 1] = r;
    return 1;
}

__declspec(dllexport) int __cdecl TDX_STRICT(
    int n, float* pfOut,
    float* p1, float* p2, float* p3, float* p4)
{
    int i;
    float r = 0;
    for (i = 0; i < n; i++) pfOut[i] = 0;
    DoSEPA(p1, p2, p3, p4, n, 1, &r);
    if (n > 0) pfOut[n - 1] = r;
    return 1;
}

__declspec(dllexport) int __cdecl TDX_EXEC(
    int n, float* pfOut,
    float* p1, float* p2, float* p3, float* p4)
{
    int i;
    float r = 0;
    for (i = 0; i < n; i++) pfOut[i] = 0;
    DoSEPA(p1, p2, p3, p4, n, 2, &r);
    if (n > 0) pfOut[n - 1] = r;
    return 1;
}

__declspec(dllexport) int __cdecl TDX_SCORE(
    int n, float* pfOut,
    float* p1, float* p2, float* p3, float* p4)
{
    int i;
    float r = 0;
    for (i = 0; i < n; i++) pfOut[i] = 0;
    DoSEPA(p1, p2, p3, p4, n, 0, &r);
    if (n > 0) pfOut[n - 1] = r;
    return 1;
}

__declspec(dllexport) int __cdecl TDX_TEST(
    int n, float* pfOut,
    float* p1, float* p2, float* p3, float* p4)
{
    (void)p1; (void)p2; (void)p3; (void)p4;
    if (n > 0) pfOut[n - 1] = 42.0f;
    return 1;
}
