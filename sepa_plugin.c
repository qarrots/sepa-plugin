/* sepa_plugin.c — SEPA选股DLL (完整版) */

#include <windows.h>
#include <string.h>
#include <math.h>

#pragma pack(push, 1)
typedef struct {
    DWORD m_dwOpen;
    DWORD m_dwHigh;
    DWORD m_dwLow;
    DWORD m_dwClose;
    DWORD m_dwVolume;
    DWORD m_dwAmount;
    WORD  m_wAdvance;
    WORD  m_wDecline;
} STKDATA;

typedef struct {
    DWORD    m_dwVersion;
    DWORD    m_dwSerial;
    LPCSTR   m_strCode;
    BOOL     m_bStockIndex;
    int      m_nPeriod;
    int      m_nNumData;
    STKDATA* m_pData;
    float*   m_pfFinData;
    int      m_nCurIndex;
    int      m_nWriteIndex;
    float*   m_pfOut1;
    float*   m_pfOut2;
    float*   m_pfOut3;
    float*   m_pfOut4;
    float*   m_pfOut5;
} CALCINFO;
#pragma pack(pop)

#define MAX_STOCKS 5000
typedef struct {
    char  code[12];
    float rs;
    float rs_rank;
    int   trend_count;
    int   total_score;
    int   trend_score;
    int   rs_score;
    int   pivot_score;
    int   vcp_score;
    BOOL  stage2;
    BOOL  breakout;
    BOOL  near_pivot;
    BOOL  vcp;
    BOOL  valid;
} StockRec;

static StockRec g_data[MAX_STOCKS];
static int      g_count  = 0;
static BOOL     g_ranked = FALSE;

static float PRICE(DWORD dw) { return (float)dw / 100.0f; }
static float CL(STKDATA* d, int i) { return PRICE(d[i].m_dwClose); }
static float HI(STKDATA* d, int i) { return PRICE(d[i].m_dwHigh); }
static float LO(STKDATA* d, int i) { return PRICE(d[i].m_dwLow); }
static float VO(STKDATA* d, int i) { return (float)d[i].m_dwVolume; }

static float MA(STKDATA* d, int pos, int n) {
    if (pos < n - 1) return 0;
    float s = 0;
    for (int i = 0; i < n; i++) s += CL(d, pos - i);
    return s / n;
}

static float VMA(STKDATA* d, int pos, int n) {
    if (pos < n - 1) return 0;
    float s = 0;
    for (int i = 0; i < n; i++) s += VO(d, pos - i);
    return s / n;
}

static float HHV(STKDATA* d, int pos, int n) {
    int start = (pos >= n - 1) ? pos - n + 1 : 0;
    float hi = 0;
    for (int i = start; i <= pos; i++)
        if (HI(d, i) > hi) hi = HI(d, i);
    return hi;
}

static float LLV(STKDATA* d, int pos, int n) {
    int start = (pos >= n - 1) ? pos - n + 1 : 0;
    float lo = 999999;
    for (int i = start; i <= pos; i++)
        if (LO(d, i) < lo) lo = LO(d, i);
    return lo;
}

static StockRec* GetRec(const char* code) {
    int i;
    for (i = 0; i < g_count; i++)
        if (strcmp(g_data[i].code, code) == 0) return &g_data[i];
    if (g_count >= MAX_STOCKS) return NULL;
    {
        StockRec* r = &g_data[g_count++];
        memset(r, 0, sizeof(StockRec));
        strncpy(r->code, code, 10);
        return r;
    }
}

static StockRec* FindRec(const char* code) {
    int i;
    for (i = 0; i < g_count; i++)
        if (strcmp(g_data[i].code, code) == 0) return &g_data[i];
    return NULL;
}

static int CmpRS(const void* a, const void* b) {
    float d = ((StockRec*)b)->rs - ((StockRec*)a)->rs;
    return (d > 0) ? 1 : (d < 0) ? -1 : 0;
}

static void CalcStock(StockRec* r, STKDATA* d, int n) {
    int L, cnt = 0;
    float C, ma50, ma150, ma200, ma200p;
    float g126, g063, g021, g250;
    float a20, a40, a60, v10, v30, v50;
    float piv, dist;
    BOOL vcp_flag;

    r->valid = 0;
    if (n < 250) return;

    L = n - 1;
    C = CL(d, L);

    ma50  = MA(d, L, 50);
    ma150 = MA(d, L, 150);
    ma200 = MA(d, L, 200);
    ma200p = (n >= 222) ? MA(d, L - 22, 200) : 0;

    if (C > ma150 && C > ma200) cnt++;
    if (ma150 > ma200) cnt++;
    if (ma200 > ma200p && ma200p > 0) cnt++;
    if (ma50 > ma150 && ma50 > ma200) cnt++;
    if (C > ma50) cnt++;
    if (C >= LLV(d, L, 250) * 1.25f) cnt++;
    if (C >= HHV(d, L, 250) * 0.75f) cnt++;

    r->trend_count = cnt;
    r->stage2 = (cnt == 7);

    g126 = (n > 126) ? C / CL(d, L - 126) - 1 : 0;
    g063 = (n > 63)  ? C / CL(d, L - 63)  - 1 : 0;
    g021 = (n > 21)  ? C / CL(d, L - 21)  - 1 : 0;
    g250 = (n > 250) ? C / CL(d, L - 250) - 1 : 0;
    r->rs = 0.40f * g126 + 0.20f * g063 + 0.20f * g021 + 0.20f * g250;

    a20 = (HHV(d, L, 20) - LLV(d, L, 20)) / (HHV(d, L, 20) + 0.001f) * 100;
    a40 = (HHV(d, L, 40) - LLV(d, L, 40)) / (HHV(d, L, 40) + 0.001f) * 100;
    a60 = (HHV(d, L, 60) - LLV(d, L, 60)) / (HHV(d, L, 60) + 0.001f) * 100;
    v10 = VMA(d, L, 10);
    v30 = VMA(d, L, 30);
    v50 = VMA(d, L, 50);

    vcp_flag = (a20 < a40 && a40 < a60) && (v10 < v30 && v30 < v50);
    r->vcp = vcp_flag;

    piv = HHV(d, L - 5, 55);
    dist = (C / (piv + 0.001f) - 1) * 100;
    r->breakout = (C > piv) && (VO(d, L) > v50);
    r->near_pivot = (dist >= 0 && dist <= 3);

    /* 评分 */
    if (cnt == 7 && r->rs > 0.30f) r->trend_score = 35;
    else if (cnt == 7 && r->rs > 0.15f) r->trend_score = 27;
    else if (cnt >= 5) r->trend_score = 20;
    else if (cnt >= 4) r->trend_score = 10;
    else r->trend_score = 0;

    if (r->rs > 0.50f) r->rs_score = 25;
    else if (r->rs > 0.30f) r->rs_score = 20;
    else if (r->rs > 0.15f) r->rs_score = 15;
    else if (r->rs > 0) r->rs_score = 10;
    else if (r->rs > -0.1f) r->rs_score = 5;
    else r->rs_score = 0;

    r->vcp_score = vcp_flag ? 25 : ((a20 < a40 || v10 < v30) ? 12 : 0);

    if (r->breakout && VO(d, L) > v50 * 1.5f) r->pivot_score = 15;
    else if (r->breakout) r->pivot_score = 12;
    else if (r->near_pivot) r->pivot_score = 8;
    else if (dist > -5) r->pivot_score = 4;
    else r->pivot_score = 0;

    r->total_score = r->trend_score + r->rs_score
                   + r->pivot_score + r->vcp_score;
    r->valid = 1;
}

__declspec(dllexport) BOOL WINAPI TDX_SEPA_RESET(CALCINFO* p) {
    g_count = 0;
    g_ranked = FALSE;
    return TRUE;
}

__declspec(dllexport) BOOL WINAPI TDX_SEPA_SCAN(CALCINFO* p) {
    StockRec* r;
    if (!p || !p->m_pData || p->m_nNumData < 250) return FALSE;
    if (p->m_strCode && strstr(p->m_strCode, "ST")) return FALSE;
    r = GetRec(p->m_strCode);
    if (!r) return FALSE;
    CalcStock(r, p->m_pData, p->m_nNumData);
    if (p->m_pfOut1) p->m_pfOut1[p->m_nNumData - 1] = r->rs;
    return TRUE;
}

__declspec(dllexport) BOOL WINAPI TDX_SEPA_RANK(CALCINFO* p) {
    int i;
    if (g_count == 0) return FALSE;
    qsort(g_data, g_count, sizeof(StockRec), CmpRS);
    for (i = 0; i < g_count; i++)
        g_data[i].rs_rank = (1.0f - (float)i / g_count) * 99.0f;
    g_ranked = TRUE;
    return TRUE;
}

__declspec(dllexport) BOOL WINAPI TDX_SEPA_SEL(CALCINFO* p) {
    StockRec* r;
    int L;
    if (!p || !p->m_pfOut1) return FALSE;
    L = p->m_nNumData - 1;
    r = FindRec(p->m_strCode);
    if (!r || !r->valid) {
        if (p->m_pfOut1) p->m_pfOut1[L] = 0;
        if (p->m_pfOut2) p->m_pfOut2[L] = 0;
        if (p->m_pfOut3) p->m_pfOut3[L] = 0;
        return FALSE;
    }
    if (p->m_pfOut1) p->m_pfOut1[L] = (float)r->total_score;
    if (p->m_pfOut2) p->m_pfOut2[L] = r->rs_rank;
    if (p->m_pfOut3) p->m_pfOut3[L] =
        (r->trend_count >= 6 && r->rs_rank >= 50.0f) ? 1.0f : 0.0f;
    return TRUE;
}

__declspec(dllexport) BOOL WINAPI TDX_TEST(CALCINFO* p) {
    if (p && p->m_pfOut1)
        p->m_pfOut1[p->m_nNumData - 1] = 42.0f;
    return TRUE;
}

BOOL APIENTRY DllMain(HMODULE h, DWORD reason, LPVOID lp) {
    return TRUE;
}
