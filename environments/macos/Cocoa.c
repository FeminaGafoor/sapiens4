#include <objc/message.h>
#include <objc/runtime.h>
#include <CoreGraphics/CoreGraphics.h>

void *sapiens_objc_rect(void *object, void *selector, double x, double y, double w, double h) {
    return ((void *(*)(id, SEL, CGRect))objc_msgSend)(object, selector, CGRectMake(x,y,w,h));
}
void *sapiens_objc_rect_object(void *object, void *selector, double x, double y, double w, double h, void *value) {
    return ((void *(*)(id, SEL, CGRect, id))objc_msgSend)(object, selector, CGRectMake(x,y,w,h), value);
}
void *sapiens_objc_window(void *object, void *selector, double w, double h, unsigned long style) {
    return ((void *(*)(id, SEL, CGRect, unsigned long, unsigned long, BOOL))objc_msgSend)(object, selector, CGRectMake(0,0,w,h), style, 2, 0);
}
void sapiens_objc_size(void *object, void *selector, double w, double h) {
    ((void (*)(id, SEL, CGSize))objc_msgSend)(object, selector, CGSizeMake(w,h));
}
double sapiens_objc_height(void *object, void *selector) {
#if defined(__x86_64__)
    return ((CGRect (*)(id, SEL))objc_msgSend_stret)(object, selector).size.height;
#else
    return ((CGRect (*)(id, SEL))objc_msgSend)(object, selector).size.height;
#endif
}
void sapiens_objc_draw(void *object, void *selector, double size) {
    ((void (*)(id, SEL, CGRect, CGRect, unsigned long, double))objc_msgSend)(object, selector, CGRectMake(0,0,size,size), CGRectZero, 2, 1);
}
void sapiens_objc_convert_rect(void *object, void *selector, void *source, double x, double y, double w, double h, double *result) {
#if defined(__x86_64__)
    CGRect converted = ((CGRect (*)(id, SEL, CGRect, id))objc_msgSend_stret)(object, selector, CGRectMake(x,y,w,h), source);
#else
    CGRect converted = ((CGRect (*)(id, SEL, CGRect, id))objc_msgSend)(object, selector, CGRectMake(x,y,w,h), source);
#endif
    result[0]=converted.origin.x; result[1]=converted.origin.y; result[2]=converted.size.width; result[3]=converted.size.height;
}
