
import React, { useState, useRef, useEffect } from 'react';
import { X } from 'lucide-react';
import { Button } from '@/components/ui/button';
import { cn } from '@/lib/utils';

interface TerminalProps {
  output: string;
  isVisible: boolean;
  onClose: () => void;
  isDraggable?: boolean;
}

const Terminal: React.FC<TerminalProps> = ({
  output,
  isVisible,
  onClose,
  isDraggable = false
}) => {
  const [position, setPosition] = useState({ x: 20, y: 20 });
  const [isDragging, setIsDragging] = useState(false);
  const [size, setSize] = useState({ width: 350, height: 300 });
  const terminalRef = useRef<HTMLDivElement>(null);
  const dragHandleRef = useRef<HTMLDivElement>(null);
  
  const handleMouseDown = (e: React.MouseEvent) => {
    if (!isDraggable) return;
    
    // Only initiate drag if clicked on the header
    if (dragHandleRef.current && dragHandleRef.current.contains(e.target as Node)) {
      setIsDragging(true);
      e.preventDefault(); // Prevent text selection during drag
    }
  };

  const handleMouseMove = (e: MouseEvent) => {
    if (!isDragging || !isDraggable) return;
    
    setPosition({
      x: Math.max(0, Math.min(e.clientX - 100, window.innerWidth - size.width)),
      y: Math.max(0, Math.min(e.clientY - 20, window.innerHeight - size.height))
    });
  };

  const handleMouseUp = () => {
    setIsDragging(false);
  };

  useEffect(() => {
    const handleGlobalMouseMove = (e: MouseEvent) => {
      handleMouseMove(e);
    };
    
    const handleGlobalMouseUp = () => {
      setIsDragging(false);
    };
    
    if (isDragging) {
      window.addEventListener('mousemove', handleGlobalMouseMove);
      window.addEventListener('mouseup', handleGlobalMouseUp);
    }
    
    return () => {
      window.removeEventListener('mousemove', handleGlobalMouseMove);
      window.removeEventListener('mouseup', handleGlobalMouseUp);
    };
  }, [isDragging, isDraggable]);

  useEffect(() => {
    // Save terminal size when resizing ends
    const handleResize = () => {
      if (terminalRef.current) {
        const { width, height } = terminalRef.current.getBoundingClientRect();
        setSize({ width, height });
      }
    };

    const resizeObserver = new ResizeObserver(handleResize);
    if (terminalRef.current) {
      resizeObserver.observe(terminalRef.current);
    }

    return () => {
      if (terminalRef.current) {
        resizeObserver.disconnect();
      }
    };
  }, [isVisible]);

  // If terminal is not visible, don't render anything
  if (!isVisible) return null;

  return (
    <div 
      ref={terminalRef}
      className={cn(
        "absolute border rounded-lg p-4 bg-black/90 shadow-lg",
        isDraggable ? "resize overflow-auto" : "h-[500px] w-full overflow-auto"
      )}
      style={isDraggable ? {
        width: `${size.width}px`,
        height: `${size.height}px`,
        left: `${position.x}px`,
        top: `${position.y}px`,
        zIndex: 100,
      } : undefined}
    >
      <div 
        ref={dragHandleRef}
        className="h-6 bg-black/50 absolute top-0 left-0 right-0 text-xs text-center pt-1 flex items-center justify-between px-2"
        onMouseDown={handleMouseDown}
      >
        <span className={cn("terminal-drag-handle flex-1", isDraggable && "cursor-grab")}>
          Terminal {isDraggable && "(drag to move)"}
        </span>
        <Button 
          onClick={onClose}
          variant="ghost" 
          size="icon" 
          className="h-5 w-5 p-0 hover:bg-red-500/20"
        >
          <X className="h-3 w-3" />
        </Button>
      </div>
      <pre className="text-sm overflow-auto whitespace-pre-wrap mt-6 text-green-400 resize-none">
        {output || 'Output will appear here...'}
      </pre>
      {/* Add a resize handle indicator for the bottom-right corner */}
      {isDraggable && (
        <div className="absolute bottom-0 right-0 w-4 h-4 cursor-nwse-resize resize-handle" />
      )}
    </div>
  );
};

export default Terminal;
