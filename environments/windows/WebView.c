#include <windows.h>
typedef HRESULT (STDMETHODCALLTYPE *BoundsMethod)(void *, RECT);
HRESULT hs_webview_bounds(void *controller, int left, int top, int right, int bottom) {
    RECT bounds = {left, top, right, bottom};
    return ((BoundsMethod)(*(void ***)controller)[6])(controller, bounds);
}
