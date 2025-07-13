// Pure recursive function to traverse any nested structure
const traverse = (obj, path = []) => {
    if (Array.isArray(obj)) {
        return obj.flatMap((item, index) => 
            traverse(item, [...path, index])
        );
    } else if (obj && typeof obj === 'object') {
        return Object.entries(obj).flatMap(([key, value]) => 
            traverse(value, [...path, key])
        );
    } else {
        // Leaf node - return the path and value
        return [{ path, value: obj }];
    }
};

// Usage
const settings = JSON.parse(json);
const results = traverse(settings);
results.forEach(({path, value}) => console.log(path.join('.'), value));