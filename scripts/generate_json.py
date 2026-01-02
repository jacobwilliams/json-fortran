#!/usr/bin/env python3
"""
Generate complex JSON files with deeply-nested structures and random data.

This script creates JSON files with configurable complexity, including:
- Nested objects and arrays
- Random integers, floats, strings, booleans, and null values
- Controllable depth and breadth

### Examples

```python
# Simple usage with defaults (depth=10, breadth=10)
python3 generate_json.py -o test.json

# Very deep structure (depth=50)
python3 generate_json.py -o deep.json -d 50 -b 5

# Wide but shallow (many keys, low depth)
python3 generate_json.py -o wide.json -d 3 -b 50

# Array-heavy structure
python3 generate_json.py -o arrays.json -a 0.7

# Reproducible output
python3 generate_json.py -o test.json -s 42

# Compact output
python3 generate_json.py -o compact.json --no-indent
```
"""

import json
import random
import string
import argparse
from typing import Any, Dict, List


class JSONGenerator:
    """Generator for complex JSON structures."""

    def __init__(self, max_depth: int = 10, max_breadth: int = 10,
                 array_probability: float = 0.3, null_probability: float = 0.1,
                 random_seed: int = None):
        """
        Initialize the JSON generator.

        Args:
            max_depth: Maximum nesting depth
            max_breadth: Maximum number of children per object/array
            array_probability: Probability of creating an array vs object (0.0-1.0)
            null_probability: Probability of a null value (0.0-1.0)
            random_seed: Random seed for reproducibility
        """
        self.max_depth = max_depth
        self.max_breadth = max_breadth
        self.array_probability = array_probability
        self.null_probability = null_probability

        if random_seed is not None:
            random.seed(random_seed)

    def generate_random_string(self, min_len: int = 5, max_len: int = 20) -> str:
        """Generate a random string."""
        length = random.randint(min_len, max_len)
        chars = string.ascii_letters + string.digits + ' _-'
        return ''.join(random.choice(chars) for _ in range(length))

    def generate_random_key(self) -> str:
        """Generate a random object key."""
        prefixes = ['key', 'field', 'prop', 'attr', 'data', 'value', 'item']
        return f"{random.choice(prefixes)}_{random.randint(1, 1000)}"

    def generate_primitive(self) -> Any:
        """Generate a random primitive value."""
        # Check for null first
        if random.random() < self.null_probability:
            return None

        # Choose a primitive type
        choice = random.randint(0, 4)

        if choice == 0:  # Integer
            return random.randint(-10000, 10000)
        elif choice == 1:  # Float
            return round(random.uniform(-10000.0, 10000.0), 4)
        elif choice == 2:  # String
            return self.generate_random_string()
        elif choice == 3:  # Boolean
            return random.choice([True, False])
        else:  # Another null chance
            return None

    def generate_array(self, current_depth: int) -> List[Any]:
        """Generate a random array."""
        if current_depth >= self.max_depth:
            # At max depth, only primitives
            size = random.randint(0, self.max_breadth)
            return [self.generate_primitive() for _ in range(size)]

        size = random.randint(1, self.max_breadth)
        array = []

        for _ in range(size):
            if random.random() < 0.3 and current_depth < self.max_depth:
                # Nested structure
                array.append(self.generate_value(current_depth + 1))
            else:
                # Primitive value
                array.append(self.generate_primitive())

        return array

    def generate_object(self, current_depth: int) -> Dict[str, Any]:
        """Generate a random object."""
        if current_depth >= self.max_depth:
            # At max depth, only primitives
            size = random.randint(1, self.max_breadth)
            return {self.generate_random_key(): self.generate_primitive()
                    for _ in range(size)}

        size = random.randint(1, self.max_breadth)
        obj = {}

        for _ in range(size):
            key = self.generate_random_key()

            if random.random() < 0.4 and current_depth < self.max_depth:
                # Nested structure
                obj[key] = self.generate_value(current_depth + 1)
            else:
                # Primitive value
                obj[key] = self.generate_primitive()

        return obj

    def generate_value(self, current_depth: int = 0) -> Any:
        """Generate a random JSON value (object, array, or primitive)."""
        if current_depth >= self.max_depth:
            # At max depth, only primitives
            return self.generate_primitive()

        # Decide whether to create object, array, or primitive
        choice = random.random()

        if choice < self.array_probability:
            return self.generate_array(current_depth)
        elif choice < self.array_probability + 0.4:
            return self.generate_object(current_depth)
        else:
            # Sometimes just return a primitive at any depth
            return self.generate_primitive()

    def generate(self) -> Dict[str, Any]:
        """Generate a complete JSON structure starting with a root object."""
        return self.generate_object(0)


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description='Generate complex JSON files with nested structures and random data.',
        formatter_class=argparse.ArgumentDefaultsHelpFormatter
    )

    parser.add_argument(
        '-o', '--output',
        type=str,
        default='generated.json',
        help='Output JSON file path'
    )

    parser.add_argument(
        '-d', '--max-depth',
        type=int,
        default=10,
        help='Maximum nesting depth (how deep the structure can be)'
    )

    parser.add_argument(
        '-b', '--max-breadth',
        type=int,
        default=10,
        help='Maximum number of children per object/array'
    )

    parser.add_argument(
        '-a', '--array-probability',
        type=float,
        default=0.3,
        help='Probability of creating arrays vs objects (0.0-1.0)'
    )

    parser.add_argument(
        '-n', '--null-probability',
        type=float,
        default=0.1,
        help='Probability of null values (0.0-1.0)'
    )

    parser.add_argument(
        '-s', '--seed',
        type=int,
        default=None,
        help='Random seed for reproducibility'
    )

    parser.add_argument(
        '--indent',
        type=int,
        default=2,
        help='JSON indentation spaces (use 0 for compact output)'
    )

    parser.add_argument(
        '--no-indent',
        action='store_true',
        help='Compact JSON output (no indentation)'
    )

    args = parser.parse_args()

    # Validate arguments
    if not (0.0 <= args.array_probability <= 1.0):
        parser.error('array-probability must be between 0.0 and 1.0')

    if not (0.0 <= args.null_probability <= 1.0):
        parser.error('null-probability must be between 0.0 and 1.0')

    if args.max_depth < 1:
        parser.error('max-depth must be at least 1')

    if args.max_breadth < 1:
        parser.error('max-breadth must be at least 1')

    # Create generator
    generator = JSONGenerator(
        max_depth=args.max_depth,
        max_breadth=args.max_breadth,
        array_probability=args.array_probability,
        null_probability=args.null_probability,
        random_seed=args.seed
    )

    # Generate JSON
    print(f"Generating JSON with max_depth={args.max_depth}, max_breadth={args.max_breadth}...")
    data = generator.generate()

    # Write to file
    indent = None if args.no_indent else args.indent
    with open(args.output, 'w', encoding='utf-8') as f:
        json.dump(data, f, indent=indent, ensure_ascii=False)

    print(f"JSON file written to: {args.output}")

    # Print some statistics
    json_str = json.dumps(data)
    print(f"File size: {len(json_str):,} bytes")
    print(f"Number of objects: {json_str.count('{')}")
    print(f"Number of arrays: {json_str.count('[')}")


if __name__ == '__main__':
    main()
