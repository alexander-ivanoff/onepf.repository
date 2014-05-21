package org.onepf.repository.api;

/**
 * @author Alexander Ivanoff
 */
public class Pair<T, E> {

    public T fst;
    public E snd;

    public Pair(T first, E second) {
        fst = first;
        snd = second;
    }
}
