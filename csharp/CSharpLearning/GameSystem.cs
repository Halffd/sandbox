using System;
using System.Collections.Generic;
using System.Linq;

namespace CSharpLearning
{
    // Base interface
    interface IBase
    {
        string Name { get; set; }
        int Level { get; set; }
        int Id { get; set; }
    }

    interface ISkill : IBase
    {
        float Damage { get; set; }
        float Cooldown { get; set; }
        float ManaCost { get; set; }
        float EnergyCost { get; set; }
        float HealthCost { get; set; }
        float StaminaCost { get; set; }
        float ManaRegen { get; set; }
        float EnergyRegen { get; set; }
        float HealthRegen { get; set; }
        float StaminaRegen { get; set; }
    }

    interface ICharacter : IBase
    {
        int Health { get; set; }
        int Energy { get; set; }
        int Mana { get; set; }
        int Stamina { get; set; }
        int MaxHealth { get; set; }
        int MaxEnergy { get; set; }
        int MaxMana { get; set; }
        int MaxStamina { get; set; }
    }

    class Skill : ISkill
    {
        public string Name { get; set; }
        public int Level { get; set; }
        public int Id { get; set; }
        public float Damage { get; set; }
        public float Cooldown { get; set; }
        public float ManaCost { get; set; }
        public float EnergyCost { get; set; }
        public float HealthCost { get; set; }
        public float StaminaCost { get; set; }
        public float ManaRegen { get; set; }
        public float EnergyRegen { get; set; }
        public float HealthRegen { get; set; }
        public float StaminaRegen { get; set; }

        public Skill(string name, int level, int id)
        {
            Name = name;
            Level = level;
            Id = id;
        }
    }

    class SkillTree
    {
        public List<Skill> Skills { get; set; } = new List<Skill>();
        public string TreeName { get; set; }

        public SkillTree(string treeName)
        {
            TreeName = treeName;
        }

        public void AddSkill(Skill skill)
        {
            Skills.Add(skill);
        }

        public Skill? GetSkill(string name)
        {
            if (string.IsNullOrEmpty(name))
                return null;
                
            return Skills.FirstOrDefault(x => string.Equals(x?.Name, name, StringComparison.OrdinalIgnoreCase));
        }
    }

    class Character : ICharacter
    {
        private const int MAX_LEVEL = 100;

        // IBase properties
        public string Name { get; set; }
        public int Level { get; set; }
        public int Id { get; set; }

        // ICharacter properties
        public int Health { get; set; }
        public int Energy { get; set; }
        public int Mana { get; set; }
        public int Stamina { get; set; }
        public int MaxHealth { get; set; }
        public int MaxEnergy { get; set; }
        public int MaxMana { get; set; }
        public int MaxStamina { get; set; }

        // Character-specific stuff
        public List<SkillTree> SkillTrees { get; set; } = new List<SkillTree>();
        public List<Item> Inventory { get; set; } = new List<Item>();
        public List<Item> EquippedItems { get; set; } = new List<Item>();

        public Character(string name, int level, int id)
        {
            Name = name;
            Level = Math.Min(level, MAX_LEVEL);
            Id = id;
            SetBaseStats();
        }

        private void SetBaseStats()
        {
            MaxHealth = 100 + (Level * 5);
            MaxEnergy = 100 + (Level * 3);
            MaxMana = 100 + (Level * 4);
            MaxStamina = 100 + (Level * 2);

            Health = MaxHealth;
            Energy = MaxEnergy;
            Mana = MaxMana;
            Stamina = MaxStamina;
        }

        public bool LevelUp()
        {
            if (Level >= MAX_LEVEL) return false;

            Level++;
            Random random = new Random();

            // Increase max stats
            MaxHealth += random.Next(3, 8);
            MaxEnergy += random.Next(2, 6);
            MaxMana += random.Next(2, 7);
            MaxStamina += random.Next(1, 5);

            // Restore to full on level up
            Health = MaxHealth;
            Energy = MaxEnergy;
            Mana = MaxMana;
            Stamina = MaxStamina;

            return true;
        }

        public bool IsDead() => Health <= 0;
        public bool IsAlive() => Health > 0;

        public void TakeDamage(int damage)
        {
            Health = Math.Max(0, Health - damage);
        }

        public void Heal(int amount)
        {
            Health = Math.Min(MaxHealth, Health + amount);
        }

        public void RestoreMana(int amount)
        {
            Mana = Math.Min(MaxMana, Mana + amount);
        }

        public void RestoreStamina(int amount)
        {
            Stamina = Math.Min(MaxStamina, Stamina + amount);
        }

        public int Attack()
        {
            int damage = 10 + (Level * 2);
            Stamina = Math.Max(0, Stamina - 5);
            return damage;
        }

        public int Defend()
        {
            int defense = 5 + Level;
            Stamina = Math.Max(0, Stamina - 3);
            return defense;
        }

        public bool UseSkill(Skill skill)
        {
            if (skill == null) return false;

            // Check resource costs
            if (Health <= skill.HealthCost ||
                Mana < skill.ManaCost ||
                Energy < skill.EnergyCost ||
                Stamina < skill.StaminaCost)
            {
                return false;
            }

            // Pay costs
            Health -= (int)skill.HealthCost;
            Mana -= (int)skill.ManaCost;
            Energy -= (int)skill.EnergyCost;
            Stamina -= (int)skill.StaminaCost;

            // Apply regen effects
            Health += (int)skill.HealthRegen;
            Mana += (int)skill.ManaRegen;
            Energy += (int)skill.EnergyRegen;
            Stamina += (int)skill.StaminaRegen;

            // Clamp values
            Health = Math.Min(MaxHealth, Math.Max(0, Health));
            Mana = Math.Min(MaxMana, Math.Max(0, Mana));
            Energy = Math.Min(MaxEnergy, Math.Max(0, Energy));
            Stamina = Math.Min(MaxStamina, Math.Max(0, Stamina));

            return true;
        }

        public bool EquipItem(Item item)
        {
            if (item == null || !Inventory.Contains(item)) return false;

            EquippedItems.Add(item);
            return true;
        }

        public bool UnequipItem(Item item)
        {
            return EquippedItems.Remove(item);
        }

        public void LearnSkillTree(SkillTree skillTree)
        {
            SkillTrees.Add(skillTree);
        }

        public Skill GetSkill(string name)
        {
            foreach (var tree in SkillTrees)
            {
                var skill = tree.GetSkill(name);
                if (skill != null) return skill;
            }
            return null;
        }
    }

    class Item : IBase
    {
        public string Name { get; set; }
        public int Level { get; set; }
        public int Id { get; set; }
        public int Price { get; set; }

        public Item(string name, int level, int id, int price = 0)
        {
            Name = name;
            Level = level;
            Id = id;
            Price = price;
        }
        public virtual string Use() => "Used " + Name;
    }

    class Armor : Item
    {
        public int Defense { get; set; }

        public Armor(string name, int level, int id, int defense, int price = 0)
            : base(name, level, id, price)
        {
            Defense = defense;
        }
        public override string Use() => "Equipped " + Name;
    }

    class Weapon : Item
    {
        public int Damage { get; set; }

        public Weapon(string name, int level, int id, int damage, int price = 0)
            : base(name, level, id, price)
        {
            Damage = damage;
        }
        public override string Use() => "Equipped " + Name;
    }
    // Example usage
    public class GameSystem
    {
        public static void Run()
        {
            // Create a character
            var hero = new Character("Hero", 1, 1);
            Console.WriteLine($"Created {hero.Name} - Level {hero.Level}");
            Console.WriteLine($"Stats: HP:{hero.Health} MP:{hero.Mana} EN:{hero.Energy} ST:{hero.Stamina}");

            // Create some skills
            var fireball = new Skill("Fireball", 1, 101)
            {
                Damage = 25f,
                ManaCost = 15f,
                Cooldown = 3f
            };

            var heal = new Skill("Heal", 1, 102)
            {
                HealthRegen = 30f,
                ManaCost = 10f,
                Cooldown = 2f
            };

            // Create skill tree
            var magicTree = new SkillTree("Fire Magic");
            magicTree.AddSkill(fireball);
            magicTree.AddSkill(heal);

            hero.LearnSkillTree(magicTree);

            // Test combat
            hero.TakeDamage(50);
            Console.WriteLine($"After taking damage: HP:{hero.Health}");

            hero.UseSkill(heal);
            Console.WriteLine($"After healing: HP:{hero.Health} MP:{hero.Mana}");

            // Level up
            hero.LevelUp();
            Console.WriteLine($"Leveled up! New stats: HP:{hero.Health} MP:{hero.Mana} Level:{hero.Level}");
        }
    }
}