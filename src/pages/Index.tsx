import { Link } from "react-router-dom";
import { ArrowRight, Code, Download, Globe, Terminal, Zap } from "lucide-react";
import { Button } from "@/components/ui/button";
import { Card, CardContent, CardDescription, CardTitle } from "@/components/ui/card";
import { useEffect, useRef } from "react";

const FeatureCard = ({ icon: Icon, title, description, delay = 0 }: { icon: any, title: string, description: string, delay?: number }) => (
  <Card className="bg-secondary text-secondary-foreground border-none hover:shadow-lg transition-all duration-300 hover:-translate-y-1 opacity-0" 
    style={{ animation: `fadeSlideUp 0.6s ${0.2 + delay * 0.1}s forwards` }}>
    <CardContent className="p-6">
      <div className="mb-4 flex h-12 w-12 items-center justify-center rounded-full bg-primary/10">
        <Icon className="h-6 w-6 text-primary" />
      </div>
      <CardTitle className="mb-2 text-xl">{title}</CardTitle>
      <CardDescription className="text-sm text-secondary-foreground/80">{description}</CardDescription>
    </CardContent>
  </Card>
);

const LanguageCard = ({ name, color, index = 0 }: { name: string, color: string, index?: number }) => (
  <div 
    className={`rounded-md px-3 py-1 text-sm font-medium ${color} bg-secondary hover:bg-secondary/80 transition-colors opacity-0`}
    style={{ animation: `fadeIn 0.5s ${0.3 + index * 0.05}s forwards` }}
  >
    {name}
  </div>
);

const Index = () => {
  const heroRef = useRef<HTMLDivElement>(null);
  
  useEffect(() => {
    // Add scroll reveal animations
    const observer = new IntersectionObserver(
      (entries) => {
        entries.forEach(entry => {
          if (entry.isIntersecting) {
            entry.target.classList.add('animate-appear');
          }
        });
      },
      { threshold: 0.1 }
    );
    
    document.querySelectorAll('.animate-on-scroll').forEach(el => {
      observer.observe(el);
    });
    
    // Particle animation for the hero section
    if (heroRef.current) {
      const canvas = document.createElement('canvas');
      canvas.className = 'absolute inset-0 w-full h-full z-0';
      heroRef.current.appendChild(canvas);
      
      const ctx = canvas.getContext('2d');
      if (ctx) {
        canvas.width = heroRef.current.offsetWidth;
        canvas.height = heroRef.current.offsetHeight;
        
        const particles: {x: number, y: number, size: number, speedX: number, speedY: number}[] = [];
        
        // Create particles
        for (let i = 0; i < 50; i++) {
          particles.push({
            x: Math.random() * canvas.width,
            y: Math.random() * canvas.height,
            size: Math.random() * 3 + 1,
            speedX: (Math.random() - 0.5) * 0.5,
            speedY: (Math.random() - 0.5) * 0.5
          });
        }
        
        // Animate particles
        function animate() {
          ctx.clearRect(0, 0, canvas.width, canvas.height);
          
          for (const p of particles) {
            p.x += p.speedX;
            p.y += p.speedY;
            
            if (p.x < 0 || p.x > canvas.width) p.speedX *= -1;
            if (p.y < 0 || p.y > canvas.height) p.speedY *= -1;
            
            ctx.beginPath();
            ctx.arc(p.x, p.y, p.size, 0, Math.PI * 2);
            ctx.fillStyle = 'rgba(255, 255, 255, 0.1)';
            ctx.fill();
          }
          
          // Connect nearby particles with lines
          for (let i = 0; i < particles.length; i++) {
            for (let j = i + 1; j < particles.length; j++) {
              const dx = particles[i].x - particles[j].x;
              const dy = particles[i].y - particles[j].y;
              const distance = Math.sqrt(dx * dx + dy * dy);
              
              if (distance < 100) {
                ctx.beginPath();
                ctx.moveTo(particles[i].x, particles[i].y);
                ctx.lineTo(particles[j].x, particles[j].y);
                ctx.strokeStyle = `rgba(255, 255, 255, ${0.1 - distance/1000})`;
                ctx.lineWidth = 0.5;
                ctx.stroke();
              }
            }
          }
          
          requestAnimationFrame(animate);
        }
        
        animate();
        
        // Resize handler
        window.addEventListener('resize', () => {
          if (heroRef.current) {
            canvas.width = heroRef.current.offsetWidth;
            canvas.height = heroRef.current.offsetHeight;
          }
        });
      }
    }
    
    return () => {
      observer.disconnect();
    };
  }, []);

  return (
    <div className="min-h-screen bg-background text-foreground">
      {/* Hero Section */}
      <section ref={heroRef} className="relative overflow-hidden py-20 md:py-32">
        <div className="absolute inset-0 bg-[radial-gradient(ellipse_at_center,_var(--tw-gradient-stops))] from-accent/20 via-background to-background"></div>
        <div className="container relative mx-auto px-4 z-10">
          <div className="flex flex-col items-center text-center">
            <div 
              className="inline-block rounded-full bg-accent/10 px-3 py-1 text-sm font-medium text-primary mb-6 opacity-0"
              style={{ animation: 'fadeIn 0.8s 0.2s forwards' }}
            >
              Write, Run, Share Code
            </div>
            <h1 
              className="text-4xl md:text-6xl font-bold mb-6 bg-clip-text text-transparent bg-gradient-to-r from-primary to-primary/70 opacity-0"
              style={{ animation: 'fadeIn 0.8s 0.4s forwards, pulse 4s 1s infinite' }}
            >
              codeer<span className="text-primary/90">.org</span>
            </h1>
            <p 
              className="max-w-2xl text-xl text-muted-foreground mb-10 opacity-0"
              style={{ animation: 'fadeIn 0.8s 0.6s forwards' }}
            >
              The modern code compiler platform for developers to write, test and run code in
              multiple languages directly in your browser.
            </p>
            <Link to="/compiler" className="opacity-0" style={{ animation: 'fadeIn 0.8s 0.8s forwards' }}>
              <Button size="lg" className="group">
                Open Code Editor
                <ArrowRight className="ml-2 h-4 w-4 group-hover:translate-x-1 transition-transform" />
              </Button>
            </Link>
          </div>
        </div>
      </section>

      {/* Features Section */}
      <section className="py-20 bg-background animate-on-scroll">
        <div className="container mx-auto px-4">
          <div className="text-center mb-16">
            <h2 className="text-3xl font-bold mb-4">Powerful Features</h2>
            <p className="text-muted-foreground max-w-2xl mx-auto">
              codeer.org provides a modern development environment with everything you need
            </p>
          </div>
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
            <FeatureCard
              icon={Terminal}
              title="Multiple Languages"
              description="Support for 25+ programming languages including Python, JavaScript, Java, C++, and more."
              delay={0}
            />
            <FeatureCard
              icon={Zap}
              title="Instant Execution"
              description="Run your code instantly with our powerful backend and get results in real-time."
              delay={1}
            />
            <FeatureCard
              icon={Code}
              title="Syntax Highlighting"
              description="Beautiful syntax highlighting makes your code readable and easier to understand."
              delay={2}
            />
            <FeatureCard
              icon={Download}
              title="Code Download"
              description="Download your code with a single click to continue your work offline."
              delay={3}
            />
            <FeatureCard
              icon={Globe}
              title="Cross-Platform"
              description="Write and run code from any device with a modern web browser, no installation needed."
              delay={4}
            />
            <FeatureCard
              icon={Terminal}
              title="Error Handling"
              description="Get detailed error messages and output to help you debug your code quickly."
              delay={5}
            />
          </div>
        </div>
      </section>

      {/* Languages Section */}
      <section className="py-20 bg-accent/5 animate-on-scroll">
        <div className="container mx-auto px-4">
          <div className="text-center mb-16">
            <h2 className="text-3xl font-bold mb-4">Supported Languages</h2>
            <p className="text-muted-foreground max-w-2xl mx-auto">
              codeer.org supports a wide range of programming languages
            </p>
          </div>
          <div className="flex flex-wrap justify-center gap-3 max-w-4xl mx-auto">
            <LanguageCard name="Python" color="text-blue-500" index={0} />
            <LanguageCard name="JavaScript" color="text-yellow-500" index={1} />
            <LanguageCard name="C++" color="text-purple-500" index={2} />
            <LanguageCard name="Java" color="text-orange-500" index={3} />
            <LanguageCard name="C" color="text-blue-400" index={4} />
            <LanguageCard name="Go" color="text-cyan-500" index={5} />
            <LanguageCard name="Kotlin" color="text-purple-600" index={6} />
            <LanguageCard name="Ruby" color="text-red-500" index={7} />
            <LanguageCard name="TypeScript" color="text-blue-600" index={8} />
            <LanguageCard name="SQL" color="text-yellow-600" index={9} />
            <LanguageCard name="C#" color="text-green-500" index={10} />
            <LanguageCard name="PHP" color="text-indigo-500" index={11} />
            <LanguageCard name="Rust" color="text-orange-600" index={12} />
            <LanguageCard name="Swift" color="text-pink-500" index={13} />
            <LanguageCard name="Dart" color="text-blue-500" index={14} />
            <LanguageCard name="Pascal" color="text-yellow-500" index={15} />
            <LanguageCard name="Scala" color="text-red-600" index={16} />
            <LanguageCard name="Haskell" color="text-purple-500" index={17} />
            <LanguageCard name="Perl" color="text-blue-700" index={18} />
            <LanguageCard name="Clojure" color="text-green-600" index={19} />
            <LanguageCard name="R" color="text-blue-400" index={20} />
            <LanguageCard name="Lisp" color="text-yellow-400" index={21} />
            <LanguageCard name="Bash" color="text-gray-400" index={22} />
            <LanguageCard name="Erlang" color="text-red-400" index={23} />
            <LanguageCard name="Elixir" color="text-purple-400" index={24} />
          </div>
          <div className="mt-12 text-center opacity-0" style={{ animation: 'fadeIn 0.8s 1s forwards' }}>
            <Link to="/compiler">
              <Button variant="outline" size="lg" className="group">
                Start Coding Now
                <ArrowRight className="ml-2 h-4 w-4 group-hover:translate-x-1 transition-transform" />
              </Button>
            </Link>
          </div>
        </div>
      </section>

      {/* Call to Action */}
      <section className="py-20 bg-gradient-to-br from-secondary to-secondary/70 animate-on-scroll">
        <div className="container mx-auto px-4 text-center">
          <h2 className="text-3xl md:text-4xl font-bold mb-6 text-secondary-foreground">
            Ready to start coding?
          </h2>
          <p className="text-xl text-secondary-foreground/80 mb-10 max-w-2xl mx-auto">
            Join codeer.org today and experience the most powerful online code editor
          </p>
          <Link to="/compiler">
            <Button size="lg" variant="default" className="group">
              Open Code Editor
              <ArrowRight className="ml-2 h-4 w-4 group-hover:translate-x-1 transition-transform" />
            </Button>
          </Link>
        </div>
      </section>

      {/* Footer */}
      <footer className="py-10 bg-background border-t border-border">
        <div className="container mx-auto px-4">
          <div className="flex flex-col md:flex-row justify-between items-center">
            <div className="mb-6 md:mb-0">
              <div className="text-2xl font-bold">
                codeer<span className="text-primary/70">.org</span>
              </div>
              <p className="text-sm text-muted-foreground mt-2">
                © 2025 codeer.org. All rights reserved.
              </p>
            </div>
            <div className="flex space-x-6">
              <a href="#" className="text-muted-foreground hover:text-primary transition-colors">
                Terms
              </a>
              <a href="#" className="text-muted-foreground hover:text-primary transition-colors">
                Privacy
              </a>
              <a href="#" className="text-muted-foreground hover:text-primary transition-colors">
                Contact
              </a>
              <a 
                href="https://github.com/sriox/webcompiler" 
                target="_blank" 
                rel="noopener noreferrer"
                className="text-muted-foreground hover:text-primary transition-colors"
              >
                GitHub
              </a>
            </div>
          </div>
        </div>
      </footer>

      {/* Add CSS animations */}
      <style>{`
        @keyframes fadeIn {
          from { opacity: 0; }
          to { opacity: 1; }
        }
        
        @keyframes fadeSlideUp {
          from { 
            opacity: 0;
            transform: translateY(20px);
          }
          to { 
            opacity: 1;
            transform: translateY(0);
          }
        }
        
        @keyframes pulse {
          0% { opacity: 1; }
          50% { opacity: 0.8; }
          100% { opacity: 1; }
        }
        
        .animate-appear {
          animation: fadeSlideUp 0.8s forwards;
        }
        
        .animate-on-scroll {
          opacity: 0;
          transform: translateY(20px);
          transition: opacity 0.6s, transform 0.6s;
        }
        
        .animate-on-scroll.animate-appear {
          opacity: 1;
          transform: translateY(0);
        }
      `}</style>
    </div>
  );
};

export default Index;
