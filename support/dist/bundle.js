var __getOwnPropNames = Object.getOwnPropertyNames;
var __commonJS = (cb, mod) => function __require() {
  return mod || (0, cb[__getOwnPropNames(cb)[0]])((mod = { exports: {} }).exports, mod), mod.exports;
};

// node_modules/fast-equals/dist/cjs/index.cjs
var require_cjs = __commonJS({
  "node_modules/fast-equals/dist/cjs/index.cjs"(exports) {
    "use strict";
    var { getOwnPropertyNames, getOwnPropertySymbols } = Object;
    var { hasOwnProperty } = Object.prototype;
    function combineComparators(comparatorA, comparatorB) {
      return function isEqual(a, b, state) {
        return comparatorA(a, b, state) && comparatorB(a, b, state);
      };
    }
    function createIsCircular(areItemsEqual) {
      return function isCircular(a, b, state) {
        if (!a || !b || typeof a !== "object" || typeof b !== "object") {
          return areItemsEqual(a, b, state);
        }
        const { cache } = state;
        const cachedA = cache.get(a);
        const cachedB = cache.get(b);
        if (cachedA && cachedB) {
          return cachedA === b && cachedB === a;
        }
        cache.set(a, b);
        cache.set(b, a);
        const result = areItemsEqual(a, b, state);
        cache.delete(a);
        cache.delete(b);
        return result;
      };
    }
    function getStrictProperties(object) {
      return getOwnPropertyNames(object).concat(getOwnPropertySymbols(object));
    }
    var hasOwn = (
      // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
      Object.hasOwn || ((object, property) => hasOwnProperty.call(object, property))
    );
    var PREACT_VNODE = "__v";
    var PREACT_OWNER = "__o";
    var REACT_OWNER = "_owner";
    var { getOwnPropertyDescriptor, keys } = Object;
    var sameValueEqual = (
      // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
      Object.is || function sameValueEqual2(a, b) {
        return a === b ? a !== 0 || 1 / a === 1 / b : a !== a && b !== b;
      }
    );
    function sameValueZeroEqual(a, b) {
      return a === b || a !== a && b !== b;
    }
    function strictEqual(a, b) {
      return a === b;
    }
    function areArrayBuffersEqual(a, b) {
      return a.byteLength === b.byteLength && areTypedArraysEqual(new Uint8Array(a), new Uint8Array(b));
    }
    function areArraysEqual(a, b, state) {
      let index = a.length;
      if (b.length !== index) {
        return false;
      }
      while (index-- > 0) {
        if (!state.equals(a[index], b[index], index, index, a, b, state)) {
          return false;
        }
      }
      return true;
    }
    function areDataViewsEqual(a, b) {
      return a.byteLength === b.byteLength && areTypedArraysEqual(new Uint8Array(a.buffer, a.byteOffset, a.byteLength), new Uint8Array(b.buffer, b.byteOffset, b.byteLength));
    }
    function areDatesEqual(a, b) {
      return sameValueEqual(a.getTime(), b.getTime());
    }
    function areErrorsEqual(a, b) {
      return a.name === b.name && a.message === b.message && a.cause === b.cause && a.stack === b.stack;
    }
    function areMapsEqual(a, b, state) {
      const size = a.size;
      if (size !== b.size) {
        return false;
      }
      if (!size) {
        return true;
      }
      const matchedIndices = new Array(size);
      const aIterable = a.entries();
      let aResult;
      let bResult;
      let index = 0;
      while (aResult = aIterable.next()) {
        if (aResult.done) {
          break;
        }
        const bIterable = b.entries();
        let hasMatch = false;
        let matchIndex = 0;
        while (bResult = bIterable.next()) {
          if (bResult.done) {
            break;
          }
          if (matchedIndices[matchIndex]) {
            matchIndex++;
            continue;
          }
          const aEntry = aResult.value;
          const bEntry = bResult.value;
          if (state.equals(aEntry[0], bEntry[0], index, matchIndex, a, b, state) && state.equals(aEntry[1], bEntry[1], aEntry[0], bEntry[0], a, b, state)) {
            hasMatch = matchedIndices[matchIndex] = true;
            break;
          }
          matchIndex++;
        }
        if (!hasMatch) {
          return false;
        }
        index++;
      }
      return true;
    }
    function areObjectsEqual(a, b, state) {
      const properties = keys(a);
      let index = properties.length;
      if (keys(b).length !== index) {
        return false;
      }
      while (index-- > 0) {
        if (!isPropertyEqual(a, b, state, properties[index])) {
          return false;
        }
      }
      return true;
    }
    function areObjectsEqualStrict(a, b, state) {
      const properties = getStrictProperties(a);
      let index = properties.length;
      if (getStrictProperties(b).length !== index) {
        return false;
      }
      let property;
      let descriptorA;
      let descriptorB;
      while (index-- > 0) {
        property = properties[index];
        if (!isPropertyEqual(a, b, state, property)) {
          return false;
        }
        descriptorA = getOwnPropertyDescriptor(a, property);
        descriptorB = getOwnPropertyDescriptor(b, property);
        if ((descriptorA || descriptorB) && (!descriptorA || !descriptorB || descriptorA.configurable !== descriptorB.configurable || descriptorA.enumerable !== descriptorB.enumerable || descriptorA.writable !== descriptorB.writable)) {
          return false;
        }
      }
      return true;
    }
    function arePrimitiveWrappersEqual(a, b) {
      return sameValueEqual(a.valueOf(), b.valueOf());
    }
    function areRegExpsEqual(a, b) {
      return a.source === b.source && a.flags === b.flags;
    }
    function areSetsEqual(a, b, state) {
      const size = a.size;
      if (size !== b.size) {
        return false;
      }
      if (!size) {
        return true;
      }
      const matchedIndices = new Array(size);
      const aIterable = a.values();
      let aResult;
      let bResult;
      while (aResult = aIterable.next()) {
        if (aResult.done) {
          break;
        }
        const bIterable = b.values();
        let hasMatch = false;
        let matchIndex = 0;
        while (bResult = bIterable.next()) {
          if (bResult.done) {
            break;
          }
          if (!matchedIndices[matchIndex] && state.equals(aResult.value, bResult.value, aResult.value, bResult.value, a, b, state)) {
            hasMatch = matchedIndices[matchIndex] = true;
            break;
          }
          matchIndex++;
        }
        if (!hasMatch) {
          return false;
        }
      }
      return true;
    }
    function areTypedArraysEqual(a, b) {
      let index = a.byteLength;
      if (b.byteLength !== index || a.byteOffset !== b.byteOffset) {
        return false;
      }
      while (index-- > 0) {
        if (a[index] !== b[index]) {
          return false;
        }
      }
      return true;
    }
    function areUrlsEqual(a, b) {
      return a.hostname === b.hostname && a.pathname === b.pathname && a.protocol === b.protocol && a.port === b.port && a.hash === b.hash && a.username === b.username && a.password === b.password;
    }
    function isPropertyEqual(a, b, state, property) {
      if ((property === REACT_OWNER || property === PREACT_OWNER || property === PREACT_VNODE) && (a.$$typeof || b.$$typeof)) {
        return true;
      }
      return hasOwn(b, property) && state.equals(a[property], b[property], property, property, a, b, state);
    }
    var toString = Object.prototype.toString;
    function createEqualityComparator(config) {
      const supportedComparatorMap = createSupportedComparatorMap(config);
      const { areArraysEqual: areArraysEqual2, areDatesEqual: areDatesEqual2, areFunctionsEqual, areMapsEqual: areMapsEqual2, areNumbersEqual, areObjectsEqual: areObjectsEqual2, areRegExpsEqual: areRegExpsEqual2, areSetsEqual: areSetsEqual2, getUnsupportedCustomComparator } = config;
      return function comparator(a, b, state) {
        if (a === b) {
          return true;
        }
        if (a == null || b == null) {
          return false;
        }
        const type = typeof a;
        if (type !== typeof b) {
          return false;
        }
        if (type !== "object") {
          if (type === "number" || type === "bigint") {
            return areNumbersEqual(a, b, state);
          }
          if (type === "function") {
            return areFunctionsEqual(a, b, state);
          }
          return false;
        }
        const constructor = a.constructor;
        if (constructor !== b.constructor) {
          return false;
        }
        if (constructor === Object) {
          return areObjectsEqual2(a, b, state);
        }
        if (constructor === Array) {
          return areArraysEqual2(a, b, state);
        }
        if (constructor === Date) {
          return areDatesEqual2(a, b, state);
        }
        if (constructor === RegExp) {
          return areRegExpsEqual2(a, b, state);
        }
        if (constructor === Map) {
          return areMapsEqual2(a, b, state);
        }
        if (constructor === Set) {
          return areSetsEqual2(a, b, state);
        }
        if (constructor === Promise) {
          return false;
        }
        if (Array.isArray(a)) {
          return areArraysEqual2(a, b, state);
        }
        const tag = toString.call(a);
        const supportedComparator = supportedComparatorMap[tag];
        if (supportedComparator) {
          return supportedComparator(a, b, state);
        }
        const unsupportedCustomComparator = getUnsupportedCustomComparator && getUnsupportedCustomComparator(a, b, state, tag);
        if (unsupportedCustomComparator) {
          return unsupportedCustomComparator(a, b, state);
        }
        return false;
      };
    }
    function createEqualityComparatorConfig({ circular, createCustomConfig, strict }) {
      let config = {
        areArrayBuffersEqual,
        areArraysEqual: strict ? areObjectsEqualStrict : areArraysEqual,
        areDataViewsEqual,
        areDatesEqual,
        areErrorsEqual,
        areFunctionsEqual: strictEqual,
        areMapsEqual: strict ? combineComparators(areMapsEqual, areObjectsEqualStrict) : areMapsEqual,
        areNumbersEqual: sameValueEqual,
        areObjectsEqual: strict ? areObjectsEqualStrict : areObjectsEqual,
        arePrimitiveWrappersEqual,
        areRegExpsEqual,
        areSetsEqual: strict ? combineComparators(areSetsEqual, areObjectsEqualStrict) : areSetsEqual,
        areTypedArraysEqual: strict ? combineComparators(areTypedArraysEqual, areObjectsEqualStrict) : areTypedArraysEqual,
        areUrlsEqual,
        getUnsupportedCustomComparator: void 0
      };
      if (createCustomConfig) {
        config = Object.assign({}, config, createCustomConfig(config));
      }
      if (circular) {
        const areArraysEqual2 = createIsCircular(config.areArraysEqual);
        const areMapsEqual2 = createIsCircular(config.areMapsEqual);
        const areObjectsEqual2 = createIsCircular(config.areObjectsEqual);
        const areSetsEqual2 = createIsCircular(config.areSetsEqual);
        config = Object.assign({}, config, {
          areArraysEqual: areArraysEqual2,
          areMapsEqual: areMapsEqual2,
          areObjectsEqual: areObjectsEqual2,
          areSetsEqual: areSetsEqual2
        });
      }
      return config;
    }
    function createInternalEqualityComparator(compare) {
      return function(a, b, _indexOrKeyA, _indexOrKeyB, _parentA, _parentB, state) {
        return compare(a, b, state);
      };
    }
    function createIsEqual({ circular, comparator, createState, equals, strict }) {
      if (createState) {
        return function isEqual(a, b) {
          const { cache = circular ? /* @__PURE__ */ new WeakMap() : void 0, meta } = createState();
          return comparator(a, b, {
            cache,
            equals,
            meta,
            strict
          });
        };
      }
      if (circular) {
        return function isEqual(a, b) {
          return comparator(a, b, {
            cache: /* @__PURE__ */ new WeakMap(),
            equals,
            meta: void 0,
            strict
          });
        };
      }
      const state = {
        cache: void 0,
        equals,
        meta: void 0,
        strict
      };
      return function isEqual(a, b) {
        return comparator(a, b, state);
      };
    }
    function createSupportedComparatorMap({ areArrayBuffersEqual: areArrayBuffersEqual2, areArraysEqual: areArraysEqual2, areDataViewsEqual: areDataViewsEqual2, areDatesEqual: areDatesEqual2, areErrorsEqual: areErrorsEqual2, areFunctionsEqual, areMapsEqual: areMapsEqual2, areNumbersEqual, areObjectsEqual: areObjectsEqual2, arePrimitiveWrappersEqual: arePrimitiveWrappersEqual2, areRegExpsEqual: areRegExpsEqual2, areSetsEqual: areSetsEqual2, areTypedArraysEqual: areTypedArraysEqual2, areUrlsEqual: areUrlsEqual2 }) {
      return {
        "[object Arguments]": areObjectsEqual2,
        "[object Array]": areArraysEqual2,
        "[object ArrayBuffer]": areArrayBuffersEqual2,
        "[object AsyncGeneratorFunction]": areFunctionsEqual,
        "[object BigInt]": areNumbersEqual,
        "[object BigInt64Array]": areTypedArraysEqual2,
        "[object BigUint64Array]": areTypedArraysEqual2,
        "[object Boolean]": arePrimitiveWrappersEqual2,
        "[object DataView]": areDataViewsEqual2,
        "[object Date]": areDatesEqual2,
        // If an error tag, it should be tested explicitly. Like RegExp, the properties are not
        // enumerable, and therefore will give false positives if tested like a standard object.
        "[object Error]": areErrorsEqual2,
        "[object Float16Array]": areTypedArraysEqual2,
        "[object Float32Array]": areTypedArraysEqual2,
        "[object Float64Array]": areTypedArraysEqual2,
        "[object Function]": areFunctionsEqual,
        "[object GeneratorFunction]": areFunctionsEqual,
        "[object Int8Array]": areTypedArraysEqual2,
        "[object Int16Array]": areTypedArraysEqual2,
        "[object Int32Array]": areTypedArraysEqual2,
        "[object Map]": areMapsEqual2,
        "[object Number]": arePrimitiveWrappersEqual2,
        "[object Object]": (a, b, state) => (
          // The exception for value comparison is custom `Promise`-like class instances. These should
          // be treated the same as standard `Promise` objects, which means strict equality, and if
          // it reaches this point then that strict equality comparison has already failed.
          typeof a.then !== "function" && typeof b.then !== "function" && areObjectsEqual2(a, b, state)
        ),
        // For RegExp, the properties are not enumerable, and therefore will give false positives if
        // tested like a standard object.
        "[object RegExp]": areRegExpsEqual2,
        "[object Set]": areSetsEqual2,
        "[object String]": arePrimitiveWrappersEqual2,
        "[object URL]": areUrlsEqual2,
        "[object Uint8Array]": areTypedArraysEqual2,
        "[object Uint8ClampedArray]": areTypedArraysEqual2,
        "[object Uint16Array]": areTypedArraysEqual2,
        "[object Uint32Array]": areTypedArraysEqual2
      };
    }
    var deepEqual = createCustomEqual();
    var strictDeepEqual = createCustomEqual({ strict: true });
    var circularDeepEqual = createCustomEqual({ circular: true });
    var strictCircularDeepEqual = createCustomEqual({
      circular: true,
      strict: true
    });
    var shallowEqual = createCustomEqual({
      createInternalComparator: () => sameValueEqual
    });
    var strictShallowEqual = createCustomEqual({
      strict: true,
      createInternalComparator: () => sameValueEqual
    });
    var circularShallowEqual = createCustomEqual({
      circular: true,
      createInternalComparator: () => sameValueEqual
    });
    var strictCircularShallowEqual = createCustomEqual({
      circular: true,
      createInternalComparator: () => sameValueEqual,
      strict: true
    });
    function createCustomEqual(options = {}) {
      const { circular = false, createInternalComparator: createCustomInternalComparator, createState, strict = false } = options;
      const config = createEqualityComparatorConfig(options);
      const comparator = createEqualityComparator(config);
      const equals = createCustomInternalComparator ? createCustomInternalComparator(comparator) : createInternalEqualityComparator(comparator);
      return createIsEqual({ circular, comparator, createState, equals, strict });
    }
    exports.circularDeepEqual = circularDeepEqual;
    exports.circularShallowEqual = circularShallowEqual;
    exports.createCustomEqual = createCustomEqual;
    exports.deepEqual = deepEqual;
    exports.sameValueEqual = sameValueEqual;
    exports.sameValueZeroEqual = sameValueZeroEqual;
    exports.shallowEqual = shallowEqual;
    exports.strictCircularDeepEqual = strictCircularDeepEqual;
    exports.strictCircularShallowEqual = strictCircularShallowEqual;
    exports.strictDeepEqual = strictDeepEqual;
    exports.strictEqual = strictEqual;
    exports.strictShallowEqual = strictShallowEqual;
  }
});

// index.js
(function() {
  const { deepEqual } = require_cjs();
  globalThis.runtime = {
    deepEqual
  };
})();
