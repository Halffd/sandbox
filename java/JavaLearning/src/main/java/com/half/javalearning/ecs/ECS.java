package com.half.javalearning.ecs;

import java.util.*;
import java.util.stream.Collectors;

// Entity is just an ID
class Entity {
    private static int nextId = 0;
    public final int id;

    public Entity() {
        this.id = nextId++;
    }
}

// Component marker interface
interface Component {}

// Actual components - just data
class Position implements Component {
    public float x, y;
    public Position(float x, float y) { this.x = x; this.y = y; }
}

class Velocity implements Component {
    public float dx, dy;
    public Velocity(float dx, float dy) { this.dx = dx; this.dy = dy; }
}

class Health implements Component {
    public int hp;
    public Health(int hp) { this.hp = hp; }
}

// World manages everything
class World {
    private Map<Integer, Entity> entities = new HashMap<>();
    private Map<Class<? extends Component>, Map<Integer, Component>> components = new HashMap<>();

    public Entity createEntity() {
        Entity entity = new Entity();
        entities.put(entity.id, entity);
        return entity;
    }

    public <T extends Component> void addComponent(Entity entity, T component) {
        components.computeIfAbsent(component.getClass(), k -> new HashMap<>())
                .put(entity.id, component);
    }

    @SuppressWarnings("unchecked")
    public <T extends Component> T getComponent(Entity entity, Class<T> type) {
        return (T) components.getOrDefault(type, Collections.emptyMap()).get(entity.id);
    }

    public List<Entity> getEntitiesWith(Class<? extends Component>... componentTypes) {
        return entities.values().stream()
                .filter(entity -> Arrays.stream(componentTypes)
                        .allMatch(type -> components.getOrDefault(type, Collections.emptyMap())
                                .containsKey(entity.id)))
                .collect(Collectors.toList());
    }
}

// Systems process entities
abstract class System {
    protected World world;

    public System(World world) { this.world = world; }
    public abstract void update(float deltaTime);
}

class MovementSystem extends System {
    public MovementSystem(World world) { super(world); }

    @Override
    public void update(float deltaTime) {
        for (Entity entity : world.getEntitiesWith(Position.class, Velocity.class)) {
            Position pos = world.getComponent(entity, Position.class);
            Velocity vel = world.getComponent(entity, Velocity.class);

            pos.x += vel.dx * deltaTime;
            pos.y += vel.dy * deltaTime;
        }
    }
}
public class ECS {
    public static void main(String[] args) {
// Usage
        World world = new World();
        Entity player = world.createEntity();
        world.addComponent(player, new Position(0, 0));
        world.addComponent(player, new Velocity(5, 0));
        world.addComponent(player, new Health(100));

        MovementSystem movement = new MovementSystem(world);
        movement.update(0.016f); // Player moves right
    }
}