
%-record(hull, {cargoSpace, mass, volume}).
-record(world, {zones}).
-record(zone, {name, objects, clients}).
-record(user, {name, home_zone_name}).
-record(client, {name, ship_pid, zone_pid, frontend, active}).
-record(ship, {name, x, y, turn, direction, acceleration, velocity, captain_id, health, zone_pid}).
