(in-package #:3b-openxr-parse-spec2)
(defvar *defines-xmlval*
  (alexandria:plist-hash-table
   '(:MAKE-VERSION "#define XR_MAKE_VERSION(major, minor, patch) \\
    ((((major) & 0xffffULL) << 48) | (((minor) & 0xffffULL) << 32) | ((patch) & 0xffffffffULL))"
     :VERSION-MAJOR "#define XR_VERSION_MAJOR(version) (uint16_t)(((uint64_t)(version) >> 48)& 0xffffULL)"
     :VERSION-MINOR "#define XR_VERSION_MINOR(version) (uint16_t)(((uint64_t)(version) >> 32) & 0xffffULL)"
     :VERSION-PATCH "#define XR_VERSION_PATCH(version) (uint32_t)((uint64_t)(version) & 0xffffffffULL)"
     :CURRENT-API-VERSION "// OpenXR current version number.
#define XR_CURRENT_API_VERSION XR_MAKE_VERSION(1, 0, 27)"
     :MAY-ALIAS "#if !defined(XR_MAY_ALIAS)
#if defined(__clang__) || (defined(__GNUC__) && (__GNUC__ > 4))
#define XR_MAY_ALIAS __attribute__((__may_alias__))
#else
#define XR_MAY_ALIAS
#endif
#endif"
     :DEFINE-HANDLE "#if !defined(XR_DEFINE_HANDLE)
#if (XR_PTR_SIZE == 8)
    #define XR_DEFINE_HANDLE(object) typedef struct object##_T* object;
#else
    #define XR_DEFINE_HANDLE(object) typedef uint64_t object;
#endif
#endif"
     :NULL-HANDLE "#if !defined(XR_NULL_HANDLE)
#if (XR_PTR_SIZE == 8) && XR_CPP_NULLPTR_SUPPORTED
    #define XR_NULL_HANDLE nullptr
#else
    #define XR_NULL_HANDLE 0
#endif
#endif"
     :DEFINE-ATOM "#if !defined(XR_DEFINE_ATOM)
    #define XR_DEFINE_ATOM(object) typedef uint64_t object;
#endif"
     :NULL-PATH "#define XR_NULL_PATH 0"
     :NULL-SYSTEM-ID "#define XR_NULL_SYSTEM_ID 0"
     :SUCCEEDED "#define XR_SUCCEEDED(result) ((result) >= 0)"
     :UNQUALIFIED-SUCCESS "#define XR_UNQUALIFIED_SUCCESS(result) ((result) == 0)"
     :FAILED "#define XR_FAILED(result) ((result) < 0)"
     :NO-DURATION "#define XR_NO_DURATION 0"
     :INFINITE-DURATION "#define XR_INFINITE_DURATION 0x7fffffffffffffffLL"
     :MIN-HAPTIC-DURATION "#define XR_MIN_HAPTIC_DURATION -1"
     :FREQUENCY-UNSPECIFIED "#define XR_FREQUENCY_UNSPECIFIED 0"
     :MAX-EVENT-DATA-SIZE "#define XR_MAX_EVENT_DATA_SIZE sizeof(XrEventDataBuffer)"
     :openxr-platform-defines "#include \"openxr_platform_defines.h\""

     :hand-joint-count-ext "#define XR_HAND_JOINT_COUNT_EXT 26"
     :min-composition-layers-supported "#define XR_MIN_COMPOSITION_LAYERS_SUPPORTED 16"
     :null-controller-model-key-msft "#define XR_NULL_CONTROLLER_MODEL_KEY_MSFT 0"
     :null-render-model-key-fb "#define XR_NULL_RENDER_MODEL_KEY_FB 0"
     :facial-expression-eye-count-htc "#define XR_FACIAL_EXPRESSION_EYE_COUNT_HTC 14"
     :facial-expression-lip-count-htc "#define XR_FACIAL_EXPRESSION_LIP_COUNT_HTC 37"
     :HAND-FOREARM-JOINT-COUNT-ULTRALEAP "#define XR_HAND_FOREARM_JOINT_COUNT_ULTRALEAP 27"
     :FACE-EXPRESSSION-SET-DEFAULT-FB "#define XR_FACE_EXPRESSSION_SET_DEFAULT_FB XR_FACE_EXPRESSION_SET_DEFAULT_FB"
     :MAX-HAPTIC-AMPLITUDE-ENVELOPE-SAMPLES-FB "#define XR_MAX_HAPTIC_AMPLITUDE_ENVELOPE_SAMPLES_FB 4000u"
     :MAX-HAPTIC-PCM-BUFFER-SIZE-FB "#define XR_MAX_HAPTIC_PCM_BUFFER_SIZE_FB 4000")))
