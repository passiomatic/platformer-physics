#!/usr/bin/env python3
import os
import sys
import json
import struct
import base64
import io
from urllib.parse import urlparse
import itertools
from functools import partial
from optparse import OptionParser, make_option
from PIL import Image
import math

BOILERPLATE = """
module Levels exposing (..)

import AltMath.Vector2 exposing (Vec2, vec2)
import Entity exposing (EntityType(..), PlayerData)


"""

# Tiled render order
RIGHT_DOWN, RIGHT_UP, LEFT_DOWN, LEFT_UP = "right-down", "right-up", "left-down", "left-up"

# Serializers for basic Elm types


def serialize_string(v):
    return '"%s"' % v


def serialize_float(v):
    return '%f' % v


def serialize_int(v):
    return '%d' % v


def serialize_bool(v):
    return "True" if v else "False"

# Collection/aggregated types


def serialize_list(fields):
    return "[ " + ", ".join(map(str, fields)) + " ]"


def serialize_record(fields):
    l = ["%s = %s" % (k, serialize_value(v)) for (k, v) in fields.items()]
    return "{ " + ", ".join(l) + "}"


def serialize_ctor(type_, fields):
    l = ["%s" % serialize_value(field) for field in fields]
    return type_ + " " + " ".join(l)


def serialize_tuple(values):
    l = [serialize_value(v) for v in values]
    return "( " + ", ".join(l) + " )"

# Pass value as-is


class Identity(object):
    def __init__(self, v):
        self.v = v

    def __str__(self):
        return self.serialize('')

    def serialize(self, _):
        return self.v

# 2D vector


class Vec2(object):
    def __init__(self, v):
        self.v = v

    def serialize(self, _):
        x, y = self.v
        return 'vec2 %d %d' % (x, y)

    def __str__(self):
        return self.serialize('')

# The top serializer, this calls all the serializers above.
#   Lists, records and tuple serializers can also call
#   serialize_value() for each element


def serialize_value(v):
    t = type(v)
    if t == str:
        serializer = serialize_string
    elif t == int:
        serializer = serialize_int
    elif t == float:
        serializer = serialize_float
    elif t == list:
        serializer = serialize_list
    elif t == bool:
        serializer = serialize_bool
    elif t == tuple:
        serializer = serialize_tuple
    elif t == dict:
        serializer = serialize_record
    # Special serializers
    elif t in (Identity, Vec2):
        serializer = v.serialize
    else:
        raise TypeError("Cannot find serializer for given type {t}")

    return serializer(v)

# -------------
# Tiled stuff
# -------------

def serialize_objects_layer(level, objects):

    # Convert size in pixels
    level_w = level['width'] * level['tilewidth']
    level_h = level['height'] * level['tileheight']
    order = level['renderorder']

    def convert_object(object):

        # Check object type
        if "polyline" in object:
            origin_x, origin_y = convert_point_position(level_w, level_h, object['x'], object['y'])
            converter = partial(convert_polygon_position, origin_x, origin_y)

            # Convert a polyline into a list of segments, e.g.:
            #   A three point A-B-C polyline becomes two A-B and B-C segments.

            points = []
            start_point = object['polyline'][0]
            for point in object['polyline'][1:]:
                p1 = Vec2(converter(start_point['x'], start_point['y']))
                p2 = Vec2(converter(point['x'], point['y']))
                vx = p2.v[0] - p1.v[0]
                vy = p2.v[1] - p1.v[1]
                # Rotate 90 ccw
                rx = -vy
                ry = vx
                length = math.sqrt(rx * rx + ry * ry)
                nx = rx / length
                ny = ry / length
                normal = Vec2((nx, ny))
                points.append(serialize_record({'p1': p1, 'p2': p2, 'normal': normal}))
                start_point = point

            return points

        # A point is intended as spawn position for an entity.

        elif "point" in object:
            name = object['name']
            p = convert_point_position(level_w, level_h, object['x'], object['y'])
            if name == "Player":
                type_ = Identity("Player (PlayerData 0)")
            else:
                type_ = Identity(name)
            # Always facing right
            return [serialize_record({'position': Vec2(p), 'side':  Identity("1"), 'type_': type_})]

        # In the end assume a rectangle object, which is intended as a platform 

        else:
            originX, originY = convert_rect_position(order, level_w, level_h, object['width'], object['height'], object['x'], object['y'])
            w = object['width']
            h = object['height']
            return [serialize_record({'position': Vec2((originX, originY)), "width": int(w),  'height': int(h), 'maxOffset': get_property(object, "maxOffset", 100), 'period': get_property(object, "period", 5)})]

    all_points = []
    for object in objects:
        all_points.extend(convert_object(object))

    return serialize_list(all_points)


def serialize_level(name, level):

    output = "%s = {\n" % name

    object_layers = filter(is_visible, filter(is_object_layer, level['layers']))
    objects = [(layer['name'].lower(), serialize_objects_layer(level, layer['objects'])) for layer in object_layers]

    # List all object layers found
    for index, (layer_name, layer_objects) in enumerate(objects):
        output += "    %s%s = %s\n" % ("" if index == 0 else ", ", layer_name, layer_objects)

    output += "    }\n"

    return output


def is_object_layer(layer):
    return layer['type'] == "objectgroup"


def is_visible(layer):
    return layer['visible'] == True


def serialize(name, level):
    return (BOILERPLATE) +\
        ("%s" % serialize_level(name, level))


def convert_point_position(level_w, level_h, x, y):
    # Just flip Y axis
    return x, level_h - y

def convert_rect_position(order, _, level_h, w, h, x, y):
    """Convert from rect top left coordinates (used in right-down render order) to level "midpoint" coordinates"""
    if order == RIGHT_DOWN:
        # Return midpoint x and midpoint Y with flipped axis
        return x + w / 2, level_h - y - h / 2
    else:
        raise ValueError("Unsupported rendering order " + order)

def convert_polygon_position(origin_x, origin_y, x, y):
    """Convert from relative polygon coordinates to level absolute coordinates"""
    # X and Y are relative to origin
    return origin_x + x, origin_y - y


def get_property(obj, name, default):
    """Look up for property name"""
    for index, prop in enumerate(obj["properties"]):
        if prop["name"] == name:
            try:
                # Grab the first
                return obj["properties"][index]["value"]
            except KeyError as ex:
                return default
    else:
        return default


# Entry point

USAGE = """%prog level.json output-dir

Convert a Tiled JSON level into a Levels.elm module. Only object layers are included.
"""

def main():
    parser = OptionParser(usage=USAGE)

    _, args = parser.parse_args()
    if len(args) != 2:
        parser.error('not enough arguments given')

    input_path, output_dir = args[0], args[1]
    _, filename = os.path.split(input_path)

    with open(input_path) as infile:
        name, _ = os.path.splitext(filename)

        data = json.load(infile)
        level = serialize(name, data)

        with open(os.path.join(output_dir, "Levels.elm"), "w") as outfile:
            outfile.write(level)
            print("Written " + name + " into " + output_dir)


if __name__ == "__main__":
    main()
