import java.util.*;

class Player {

    public static class Enemy implements Comparable<Enemy> {
        private String name;
        private int distance;

        public Enemy(String name, int distance) {
            this.name = name;
            this.distance = distance;
        }

        public String getName() {
            return name;
        }

        public int getDistance() {
            return distance;
        }

        @Override
        public int compareTo(Enemy other) {
            return this.getDistance() - other.getDistance();
        }
    }

    public static class Cannon {
        private PriorityQueue<Enemy> closestEnemies = new PriorityQueue<Enemy>();
        private Set<String> destroyedEnemies = new HashSet<String>();

        public void targetEnemy(Enemy enemy) {
            if (!"Nobody".equals(enemy.getName())) {
                this.closestEnemies.add(enemy);
            }
            else {
                System.out.println("Nothing to target");
            }
        }

        public void fireIfPossible() {
            Enemy enemy = closestEnemies.poll();

            if (shouldFire(enemy)) {
                fire(enemy);
            }
        }

        private void fire(Enemy enemy) {
            System.out.println(enemy.getName());
            destroyedEnemies.add(enemy.getName());
        }

        private boolean shouldFire(Enemy enemy) {
            if (enemy == null) {
                return false;
            }

            if (destroyedEnemies.contains(enemy.getName())) {
                return false;
            }

            return true;
        }
    }

    public static void main(String args[]) {
        Scanner in = new Scanner(System.in);

        Cannon cannon = new Cannon();

        while (in.hasNext()) {
            Enemy enemy = readEnemy(in);
            cannon.targetEnemy(enemy);
            cannon.fireIfPossible();
        }
    }

    private static Enemy readEnemy(Scanner in) {
        String name = in.next();
        int distance = in.nextInt();
        return new Enemy(name, distance);
    }
}