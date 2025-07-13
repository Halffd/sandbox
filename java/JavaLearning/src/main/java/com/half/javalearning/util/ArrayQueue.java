package com.half.javalearning.util;

import java.lang.reflect.Array;

public class ArrayQueue<T> {
    private T[] backingArray;
    private Class<T> clazz;
    private int front = 0;
    private int rear = 0;
    private int size = 0;
    private static final int DEFAULT_CAPACITY = 10;

    public ArrayQueue(Class<T> clazz) {
        this(clazz, DEFAULT_CAPACITY);
    }

    public ArrayQueue(Class<T> clazz, int capacity) {
        this.clazz = clazz;
        this.backingArray = (T[]) Array.newInstance(clazz, capacity);
    }

    public void enqueue(T item) {
        if (size == backingArray.length) {
            resize();
        }
        backingArray[rear] = item;
        rear = (rear + 1) % backingArray.length;
        size++;
    }

    public T dequeue() {
        if (isEmpty()) {
            throw new IllegalStateException("Queue is empty");
        }
        T item = backingArray[front];
        backingArray[front] = null; // prevent memory leak
        front = (front + 1) % backingArray.length;
        size--;
        return item;
    }

    public T peek() {
        if (isEmpty()) {
            throw new IllegalStateException("Queue is empty");
        }
        return backingArray[front];
    }

    public boolean isEmpty() {
        return size == 0;
    }

    public int size() {
        return size;
    }

    public T[] getBackingArray() {
        return backingArray;
    }

    private void resize() {
        T[] newArray = (T[]) Array.newInstance(clazz, backingArray.length * 2);
        for (int i = 0; i < size; i++) {
            newArray[i] = backingArray[(front + i) % backingArray.length];
        }
        backingArray = newArray;
        front = 0;
        rear = size;
    }
}
